<p> I was reading <a href="https://external-apps.qut.edu.au/futurelearn/resources/mm3/graph/graph-simple.html.utf8">some notes from QUT</a> about using spectral graph theory to partition a graph
using eigenvalues of unnormalised Laplacian. Their Matlab code looks like this: </p>

<pre>
% Form W = Gaussian distribution based on distances
W = zeros(100, 100);
D = zeros(100, 100);

sigma = 2;
N = 100;
for i = 1:N
    for j = 1:N
        if (j ~= i)
            % Calculate the distance between two points
            dist = norm([A(i, 1) - A(j, 1); A(i, 2) - A(j, 2)]);
            expp = exp(-dist^2 / (2 * sigma^2));
            adjacency(i, j) = 1; 
            % Add the weights to the matrix
            W(i, j) = expp; 
            % Add up the row sum as we go
            D(i, i) = D(i, i) + expp;
        end
    end
end

L = D - W;

Find the eigenpairs of L

[vec, val] = eig(L, 'vector');
% Ensure the eigenvalues and eigenvectors are sorted in ascending order
[val,ind] = sort(val);
vec = vec(:, ind);

% Plot with clusters identified by marker
% v2
figure; 
hold on;
for i = 1:N
    plot(i, vec(i, 2), ['k' plot_syms{i}]);
end
</pre>

<p> Personally, this gives me nightmares from undergrad numerical methods classes. So here's how to do
it in Haskell. Full source (including cabal config) for this post
is <a href="https://github.com/carlohamalainen/playground/tree/master/haskell/bigdata">here</a>. </p>

> module Notes where

<p> To create the <code>W</code> matrix we use <code>buildMatrix</code>: </p>

< buildW :: Double -> Matrix Double -> Matrix Double
< buildW sigma a = buildMatrix n n $ \(i,j) -> f sigma a i j
<   where
<     n = rows a
< 
<     f sigma a i j = if j /= i
<                         then expp sigma a i j
<                         else 0.0
< 
<     dist :: Matrix Double -> Int -> Int -> Double
<     dist m i j = norm2 $ fromList [ m!i!0 - m!j!0, m!i!1 - m!j!1 ]
< 
<     expp :: Double -> Matrix Double -> Int -> Int -> Double
<     expp sigma m i j = exp $ (-(dist m i j)**2)/(2*sigma**2)

<p> The <code>D</code> matrix is a diagonal matrix with each element
being the sum of a row of <code>W</code>, which is nicely expressible
by composing a few functions: </p>

< buildD :: Matrix Double -> Matrix Double
< buildD w = diag
<          . fromList
<          . map (foldVector (+) 0)
<          . toRows
<          $ w
<   where
<     n = rows w

<p> The <code>L</code> matrix is real and symmetric so we
use <a href="https://hackage.haskell.org/package/hmatrix-0.16.1.5/docs/Numeric-LinearAlgebra-HMatrix.html#v:eigSH">eigSH</a> which provides the eigenvalues in descending order. </p> 

< lapEigs :: Double -> Matrix Double -> (Vector Double, Matrix Double)
< lapEigs sigma m = eigSH l
<   where
<     w = buildW sigma m
<     d = buildD w
<     l = d - w

<p> To finish up, the Fiedler eigenvector corresponds to the second smallest eigenvalue: </p>

< fiedler :: Double -> Matrix Double -> (Double, Vector Double)
< fiedler sigma m = (val ! (n-2), vector $ concat $ toLists $ vec ¿ [n-2])
<   where
<     (val, vec) = lapEigs sigma m
<     n = rows m

<p> To plot the eigenvalues and eigenvector we use
the <a href="https://hackage.haskell.org/package/Chart-1.8/docs/Graphics-Rendering-Chart-Easy.html">Chart</a> library which uses the <a href="https://en.wikipedia.org/wiki/Cairo_(graphics)">cairo</a> backend. </p>


< doPlot "eigenvalues.png" "Eigenvalues" "eigenvalue" $ zip [0..] (reverse $ toList val)
< 
< doPlot "fiedler.png"
<        "Second eigenvalue of unnormalised Laplacian"
<        "fiedler eigenvector"
<        (zip [0..] $ toList algConnecEigVec)

<p><center><img src="https://raw.githubusercontent.com/carlohamalainen/playground/master/haskell/bigdata/eigenvalues.png" width=600></center></p>

<p><center><img src="https://raw.githubusercontent.com/carlohamalainen/playground/master/haskell/bigdata/fiedler.png" width=600></center></p>




