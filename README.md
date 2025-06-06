# ⚡ Wattshare - Energy Consumption Tokenizer

> 🌱 Transform your unused household energy into tradeable tokens and create a decentralized energy marketplace

## 🚀 Overview

Wattshare is a revolutionary smart contract platform built on Stacks that allows households to tokenize their unused energy and trade it with others. Turn your solar panels, wind turbines, or any energy-generating device into a source of income! 💰

## ✨ Key Features

- 🔋 **Energy Tokenization**: Convert unused energy into WATT tokens
- 🏪 **Energy Marketplace**: Buy and sell energy tokens with dynamic pricing
- 📱 **Device Management**: Register and manage multiple energy devices
- 👤 **User Profiles**: Track your energy trading history and reputation
- 💎 **Reputation System**: Build trust through successful transactions
- 🛡️ **Secure Trading**: Built-in safety mechanisms and platform fees

## 🛠️ Core Functions

### For Energy Producers 🏭

#### Register Your Device
```clarity
(contract-call? .Wattshare register-device "solar-panel-01" u5000 u100)
```
- `device-id`: Unique identifier for your device
- `energy-capacity`: Maximum energy capacity in watts
- `price-per-watt`: Your selling price per watt

#### Tokenize Your Energy
```clarity
(contract-call? .Wattshare tokenize-energy "solar-panel-01" u1000)
```
Convert 1000 watts of unused energy into WATT tokens! ⚡

#### List Energy for Sale
```clarity
(contract-call? .Wattshare list-energy-for-sale "solar-panel-01" u500 u120)
```

### For Energy Consumers 🏠

#### Purchase Energy Tokens
```clarity
(contract-call? .Wattshare purchase-energy "solar-panel-01" u300)
```
Buy 300 WATT tokens from another user's device

#### Check Available Energy
```clarity
(contract-call? .Wattshare get-device-info "solar-panel-01")
```

## 📊 Read-Only Functions

### Get Your WATT Balance
```clarity
(contract-call? .Wattshare get-watt-balance 'SP1234...)
```

### View User Profile
```clarity
(contract-call? .Wattshare get-user-profile 'SP1234...)
```

### Platform Statistics
```clarity
(contract-call? .Wattshare get-platform-stats)
```

### Calculate Purchase Costs
```clarity
(contract-call? .Wattshare calculate-purchase-cost "device-id" u100)
```

## 🎯 Getting Started

### 1. Deploy the Contract
```bash
clarinet deploy
```

### 2. Register Your First Device
```bash
clarinet console
```
```clarity
(contract-call? .Wattshare register-device "my-solar-panel" u3000 u150)
```

### 3. Start Tokenizing Energy
```clarity
(contract-call? .Wattshare tokenize-energy "my-solar-panel" u500)
```

### 4. List Energy for Sale
```clarity
(contract-call? .Wattshare list-energy-for-sale "my-solar-panel" u500 u150)
```

## 💡 Use Cases

- 🏘️ **Neighborhood Energy Sharing**: Share excess solar energy with neighbors
- 🏢 **Commercial Energy Trading**: Businesses can monetize unused energy capacity  
- 🌍 **Green Energy Incentives**: Promote renewable energy adoption through tokenization
- ⚖️ **Load Balancing**: Help balance energy supply and demand in local grids


