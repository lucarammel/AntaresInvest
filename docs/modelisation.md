# Modelling elements

## Revenues

* Renewables clusters and thermal are considered equivalent for the revenue computation. No Cfd or others contracts out of the market implemented.
* No reserves mechanism.
* Service system are implemented. it is an amount to be distributed in proportion to the shares allocated to the technologies of the existing powerfleet.
* The risk modelisation is a cVar on energy-only revenues implemented on the `build_market_info_supply()`. Bids is then computed on the capacity market. So it is the capacity market clearing price of the **mean** energy only revenue.
* Hypothesis costs (investment or operational) for capacity are not accurate for renewables and dsr. Need to be filled or updated.

## Investment decisions

* Batteries are not supported for investment or retirement. It may seem to work but the Antares architecture is complex : `thermal cluster` + `link to area named z_batteries` and there could be not effect.
* When investing, `unitcounts` are added to an existing Antares cluster and timeseries are updated (otherwise, there is no effect). Then, there is **no difference** between older capacities and the new entrant.
* No possibility to make a totally new entrant or new technology. Indeed, investment is based on `NPV` so revenues computed on existing capacity is compulsory.

## Capacity mechanism

* Capacity mechanism is not delayed as it is in real life for France. Auction, and incomes from the mechanism are on the same year.
* As we don't have the capacity certified computed for hydropower, it is compulsory to bypass this issue to build the supply curve with using a margin, `margin  = totalSupply - totalLoad` at peak.

## Price cap

The rule implemented is based on ACER 2017:
With `X` & `Y` parameters,
* Trigger to reach is `X(%) x priceCapInital`
* For `N` times trigger reached, `newPriceCap = N x Y(€) + priceCapInitial` with `N` stands as the round value of the mean computed on `n` times trigger is reached for each monte carlo year.
* Then, every next simulations are modified.
