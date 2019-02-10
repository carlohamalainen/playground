from hypothesis import note
from hypothesis.stateful import RuleBasedStateMachine, rule, invariant

class FarmerChickenFox(RuleBasedStateMachine):

    def __init__(self):
        self.shop  = set(['farmer', 'chicken', 'fox', 'corn'])
        self.house = set([])
        super().__init__()

    def is_fox_alone_with_chicken(self):
        for (side, other) in [(self.shop, self.house), (self.house, self.shop)]:
            if 'fox' in side and 'chicken' in side and 'farmer' in other:
                return True

    def is_chicken_alone_with_corn(self):
        for (side, other) in [(self.shop, self.house), (self.house, self.shop)]:
            if 'chicken' in side and 'corn' in side and 'farmer' in other:
                return True

    def state_ok(self):
        return not (self.is_fox_alone_with_chicken() or self.is_chicken_alone_with_corn())

    def save_state(self):
        self._shop  = set(list(self.shop))
        self._house = set(list(self.house))

    def undo_state(self):
        self.shop  = set(list(self._shop))
        self.house = set(list(self._house))

    @rule()
    def go_alone(self):
        # Farmer travels alone on the boat to the other side.

        self.save_state()

        if 'farmer' in self.shop:
            self.shop.remove('farmer')
            self.house.add('farmer')
            if not self.state_ok(): self.undo_state()
        elif 'farmer' in self.house:
            self.house.remove('farmer')
            self.shop.add('farmer')
            if not self.state_ok(): self.undo_state()
        else:
            raise ValueError('Lost the farmer in rule go_alone, this should not happen.')

    @rule()
    def take_chicken(self):
        # Farmer travels with the chicken to the other side.

        self.save_state()

        if 'farmer' in self.shop and 'chicken' in self.shop:
            self.shop.remove('farmer')
            self.shop.remove('chicken')
            self.house.add('farmer')
            self.house.add('chicken')
            if not self.state_ok(): self.undo_state()
        elif 'farmer' in self.house and 'chicken' in self.house:
            self.house.remove('farmer')
            self.house.remove('chicken')
            self.shop.add('farmer')
            self.shop.add('chicken')
            if not self.state_ok(): self.undo_state()

    @rule()
    def take_fox(self):
        # Farmer travels with the fox to the other side.

        self.save_state()

        if 'farmer' in self.shop and 'fox' in self.shop:
            self.shop.remove('farmer')
            self.shop.remove('fox')
            self.house.add('farmer')
            self.house.add('fox')
            if not self.state_ok(): self.undo_state()
        elif 'farmer' in self.house and 'fox' in self.house:
            self.house.remove('farmer')
            self.house.remove('fox')
            self.shop.add('farmer')
            self.shop.add('fox')
            if not self.state_ok(): self.undo_state()

    @rule()
    def take_corn(self):
        # Farmer travels with the corn to the other side.

        self.save_state()

        if 'farmer' in self.shop and 'corn' in self.shop:
            self.shop.remove('farmer')
            self.shop.remove('corn')
            self.house.add('farmer')
            self.house.add('corn')
            if not self.state_ok(): self.undo_state()
        elif 'farmer' in self.house and 'corn' in self.house:
            self.house.remove('farmer')
            self.house.remove('corn')
            self.shop.add('farmer')
            self.shop.add('corn')
            if not self.state_ok(): self.undo_state()

    @invariant()
    def fox_not_with_chicken(self):
        return not self.is_fox_alone_with_chicken()

    @invariant()
    def chicken_not_with_corn(self):
        return not self.is_chicken_alone_with_corn()

    @invariant()
    def not_solved(self):
        note("::: shop: {s}, house: {h}".format(s=self.shop, h=self.house))
        assert len(self.house) != 4

mcf = FarmerChickenFox.TestCase
