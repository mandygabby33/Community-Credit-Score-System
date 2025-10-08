# 🏦 Community Credit Score System

A decentralized creditworthiness platform built on Stacks blockchain that determines credit scores based on on-chain behavior rather than traditional financial institutions.

## 🌟 Features

- **📊 Dynamic Credit Scoring**: Credit scores calculated from actual repayment history and community endorsements
- **💰 Peer-to-Peer Lending**: Users can lend and borrow directly from each other
- **🤝 Community Endorsements**: Build reputation through peer endorsements
- **⚡ Real-time Updates**: Credit scores update automatically based on activity
- **🔒 Decentralized**: No central authority controls credit decisions

## 🚀 Getting Started

### Prerequisites

- Clarinet CLI installed
- Stacks wallet for testing

### Installation

```bash
git clone <repository-url>
cd community-credit-system
clarinet check
```

## 📖 Usage

### 💳 Basic Operations

**Deposit Funds**
```clarity
(contract-call? .community-credit deposit u1000000)
```

**Check Credit Score**
```clarity
(contract-call? .community-credit calculate-credit-score 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7)
```

**Request a Loan**
```clarity
(contract-call? .community-credit request-loan u500000 u10 u1000)
```

### 🏗️ Core Functions

| Function | Description | Parameters |
|----------|-------------|------------|
| `deposit` | Add STX to lending pool | `amount: uint` |
| `withdraw` | Remove STX from pool | `amount: uint` |
| `request-loan` | Request loan with terms | `amount: uint, interest-rate: uint, duration-blocks: uint` |
| `fund-loan` | Fund an existing loan request | `loan-id: uint` |
| `repay-loan` | Repay borrowed amount | `loan-id: uint, amount: uint` |
| `endorse-user` | Endorse another user | `user: principal` |

### 📊 Credit Score Calculation

Credit scores range from 300-1000 and are calculated based on:

- **Repayment Ratio** (30%): Total repaid vs total borrowed
- **Success Rate** (40%): Successful repayments vs total loans
- **Community Endorsements** (30%): Peer endorsements received
- **Base Score**: 500 for new users

### 🎯 Minimum Requirements

- **Loan Eligibility**: Credit score ≥ 400
- **Endorsement Rights**: Credit score ≥ 600
- **Self-endorsement**: Not allowed
- **Duplicate endorsements**: Not allowed

## 🔍 Read-Only Functions

- `get-user-profile`: View complete user profile
- `get-loan`: Get loan details by ID
- `calculate-credit-score`: Calculate current credit score
- `get-loan-status`: Check loan status and overdue status

## 🛡️ Security Features

- Prevents self-endorsement
- Validates loan amounts and balances
- Checks authorization for all operations
- Prevents double-spending and duplicate endorsements

## 🧪 Testing

```bash
clarinet test
```

## 📝 Contract Architecture

The system uses three main data structures:

1. **User Profiles**: Track borrowing history and reputation
2. **Loans**: Store loan terms and repayment status  
3. **Endorsements**: Record peer-to-peer trust relationships

## 🤝 Contributing

1. Fork the repository
2. Create feature branch
3. Submit pull request with tests

## 📄 License

MIT License - see LICENSE file for details

---

*Built with ❤️ on Stacks blockchain*
```

**Git Commit Message:**
```
feat: implement community credit score system with P2P lending and endorsements
```

**GitHub Pull Request Title:**
```
🏦 Add Community Credit Score System - Decentralized Creditworthiness Platform
```

**GitHub Pull Request Description:**
```
## 🎯 Overview
This PR introduces a complete Community Credit Score System that determines creditworthiness based on on-chain behavior rather than traditional financial institutions.

## ✨ What's Added
- **Dynamic Credit Scoring Algorithm** - Calculates scores from repayment history and community endorsements
- **Peer-to-Peer Lending System** - Direct lending/borrowing between users
- **Community Endorsement System** - Build reputation through peer validation
- **Comprehensive User Profiles** - Track borrowing history, repayments, and reputation
- **Real-time Score Updates** - Automatic credit score recalculation

## 🔧 Key Features
- Credit scores range 300-1000 with 500 starting score
- Loan eligibility requires 400+ credit score
- Endorsement rights require 600+ credit score
- Prevents self-endorsement and duplicate endorsements
- Automatic overdue loan detection

## 🧪 Testing
- All core functions tested
- Edge cases covered (invalid amounts, unauthorized access)
- Security validations implemented

## 📊 Technical Details
- 150+ lines of clean Clarity code
- Three main data structures (profiles, loans, endorsements)
- Comprehensive error handling
- Gas-optimized calculations

Ready for mainnet deployment! 🚀
