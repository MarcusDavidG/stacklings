# Token Swap - AMM/DEX

Automated Market Maker (AMM) for decentralized token swaps with liquidity pools.

## Features

- **Create Liquidity Pools**: Initialize trading pairs with token reserves
- **Add Liquidity**: Provide liquidity and earn LP shares
- **Token Swaps**: Swap tokens using constant product formula
- **Remove Liquidity**: Withdraw liquidity and burn LP shares
- **Platform Fees**: 0.3% fee on all swaps

## Key Functions

### create-pool
Create a new liquidity pool for a token pair.

### add-liquidity
Add liquidity to existing pool and receive LP shares.

### swap-a-for-b / swap-b-for-a
Swap tokens with slippage protection.

### remove-liquidity
Remove liquidity by burning LP shares.

## How It Works

Uses constant product formula (x * y = k) for automated market making. Liquidity providers earn fees proportional to their pool share.

## Deployment

```bash
clarinet check
clarinet deploy --testnet
```
