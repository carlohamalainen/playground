// Sample (partial) solution to question 9.7 in Cracking the Coding Interview, 4th Edition

public class Question {
    ArrayList<HtWt> items;
    ArrayList<HtWt> lastFoundSeq;
    ArrayList<HtWt> maxSeq;

    // Returns longer sequence
    ArrayList<HtWt> seqWithMaxLength(ArrayList<HtWt> seq1,

    ArrayList<HtWt> seq2) {
        return seq1.size() > seq2.size() ? seq1 : seq2;
    }

    // Fills next seq w decreased wts&returns index of 1st unfit item.

    int fillNextSeq(int startFrom, ArrayList<HtWt> seq) {
        int firstUnfitItem = startFrom;
        if (startFrom < items.size()) {
            for (int i = 0; i < items.size(); i++) {
                HtWt item = items.get(i);
                if (i == 0 || items.get(i-1).isBefore(item)) {
                    seq.add(item);
                } else {
                    firstUnfitItem = i;
                }
            }
        }
        return firstUnfitItem;
    }

    // Find the maximum length sequence

    void findMaxSeq() {
        Collections.sort(items);
        int currentUnfit = 0;
        while (currentUnfit < items.size()) {
            ArrayList<HtWt> nextSeq = new ArrayList<HtWt>();
            int nextUnfit = fillNextSeq(currentUnfit, nextSeq);
            maxSeq = seqWithMaxLength(maxSeq, nextSeq);
            if (nextUnfit == currentUnfit) break;
            else currentUnfit = nextUnfit;
        }
    }
}
