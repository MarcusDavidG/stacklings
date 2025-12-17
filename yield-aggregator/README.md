# Yield Aggregator - Auto-Compounding Yield Optimizer

Automatically find and compound yields from multiple DeFi protocols.

## Features

- **Multi-Strategy**: Support multiple yield strategies
- **Auto-Compounding**: Automatically reinvest yields
- **Share-Based**: LP shares represent proportional ownership
- **Performance Tracking**: Track APY and total harvested
- **Strategy Management**: Pause or deprecate strategies
- **Performance Fees**: 2% fee on harvested yields

## Key Functions

### create-strategy
Deploy a new yield strategy targeting a protocol.

### deposit
Deposit funds into a strategy and receive shares.

### withdraw
Withdraw funds by burning shares.

### harvest
Harvest and compound yields from the strategy.

### compound
Auto-compound user's position.

## Use Cases

- Yield farming optimization
- Passive income generation
- DeFi portfolio management
- Risk-adjusted returns

## Deployment

```bash
clarinet check
clarinet deploy --testnet
```
