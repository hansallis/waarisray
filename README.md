# Where is Ray? ✈️

A Lamdera web application for guessing your friend Ray's location around the world. Ray, being an airline pilot, can set his current location on a map, and friends can guess where he is. Perfect for keeping track of your globe-trotting pilot friend!

## Features

- 🔐 **Telegram Web Login Authentication** - Secure authentication using Telegram
- 🗺️ **Interactive Map** - Leaflet-powered world map for location setting and guessing
- ✈️ **Pilot Controls** - Ray can start rounds and set his location
- 🎯 **Guessing Game** - Friends can make one guess per round by clicking on the map
- 📊 **Distance Calculation** - Automatic distance calculation in kilometers
- 🏆 **Results & Leaderboard** - View results sorted by accuracy
- 📱 **Mobile Friendly** - Works great as a Telegram Web App
- 📈 **Round History** - Browse previous rounds and results

## How It Works

1. **Ray starts a new round** by clicking on the map to set his current location
2. **Friends make guesses** by clicking on the map where they think Ray is
3. **Each user gets one guess** per round
4. **During the round**, players can optionally view other players' guesses
5. **Ray closes the round** when ready, revealing his actual location
6. **Results show distances** from each guess to Ray's actual location
7. **Winners are ranked** by closest distance

## Setup Instructions

### 1. Prerequisites

- [Lamdera](https://lamdera.com/) installed
- A Telegram account
- Access to create a Telegram bot

### 2. Telegram Bot Setup

1. **Create a new Telegram bot:**
   - Message [@BotFather](https://t.me/botfather) on Telegram
   - Send `/newbot`
   - Choose a name for your bot (e.g., "Where is Ray Bot")
   - Choose a username (e.g., "whereisraybot")
   - Save the bot token for later

2. **Configure the Web App:**
   - Send `/setmenubutton` to @BotFather
   - Select your bot
   - Send the URL where your app will be hosted (e.g., `https://your-app.lamdera.app`)
   - Set the button text (e.g., "Play Game")

3. **Get Ray's Telegram ID:**
   - Have Ray start a conversation with your bot
   - Send a message to the bot
   - Check the bot logs or use a Telegram ID bot to get Ray's user ID
   - Update the `rayTelegramId` in `src/Backend.elm`

### 3. Configure the Application

1. **Update Ray's Telegram ID:**
   ```elm
   -- In src/Backend.elm, line ~47
   rayTelegramId = 123456789  -- Replace with Ray's actual Telegram ID
   ```

2. **Optional: Customize the app name and styling**
   - Update styling in `head.html`
   - Modify colors and map markers in the CSS sections

### 4. Development

1. **Clone and setup:**
   ```bash
   cd waarisray
   lamdera live
   ```

2. **Test locally:**
   - The app includes test authentication for development
   - Open `http://localhost:8000` in your browser
   - Click "Login with Telegram" to use test data

### 5. Deployment

1. **Deploy to Lamdera:**
   ```bash
   lamdera deploy
   ```

2. **Get your deployed URL:**
   - Lamdera will provide a URL like `https://your-app.lamdera.app`

3. **Update Telegram bot configuration:**
   - Go back to @BotFather
   - Use `/setmenubutton` with your deployed URL

4. **Test in Telegram:**
   - Open your bot in Telegram
   - Click the menu button to launch the web app
   - Try the authentication and game flow

## File Structure

```
waarisray/
├── elm.json              # Elm dependencies
├── head.html             # HTML head content with CSS and external scripts
├── elm-pkg-js/
│   └── ports.js          # JavaScript port handlers for maps and Telegram
├── src/
│   ├── Types.elm         # All type definitions and data models
│   ├── Frontend.elm      # Frontend logic and UI
│   ├── Backend.elm       # Backend game logic and authentication
│   └── Env.elm           # Environment configuration
└── README.md            # This file
```

## Key Components

### Authentication
- Uses Telegram Web App authentication
- Automatically identifies Ray vs. other players
- Secure session management

### Map Integration
- Leaflet.js for interactive maps
- Custom markers for different types (Ray's location, guesses)
- Click-to-place functionality
- Mobile-responsive

### Game Logic
- Round-based gameplay
- One guess per player per round
- Real-time distance calculations using Haversine formula
- Round history and statistics

## Security Notes

- Telegram Web App provides secure authentication
- Ray's identity is determined by Telegram user ID
- All game state is managed server-side
- Sessions are properly isolated

## Customization

### Adding More Pilots
If you want to support multiple pilots:

1. Update the `User` type to have a role field instead of `isRay`
2. Modify the backend authentication to check for multiple pilot IDs
3. Update the UI to show the pilot's name

### Changing Map Providers
The app uses OpenStreetMap by default. To use other providers:

1. Update the tile layer URL in `index.html`
2. Make sure to comply with the provider's terms of service
3. Consider adding API keys if required

### Styling
- Main styles are in `head.html` and `Frontend.elm`
- Telegram Web App automatically adapts to dark/light themes
- Map markers can be customized in `elm-pkg-js/ports.js`

## Troubleshooting

### Common Issues

1. **Map not loading:**
   - Check browser console for errors
   - Ensure Leaflet scripts are loading correctly
   - Verify internet connection for map tiles

2. **Telegram authentication failing:**
   - Make sure the app is opened through the Telegram bot
   - Verify the bot configuration with @BotFather
   - Check that the Web App URL is correctly set

3. **Ray can't create rounds:**
   - Verify Ray's Telegram ID is correctly set in `Backend.elm`
   - Check that Ray is using the correct Telegram account

### Development Tips

- Use browser developer tools to debug port communication
- Check the Lamdera console for backend errors
- Test with multiple browser tabs to simulate multiple users

## Contributing

Feel free to submit issues and pull requests to improve the game!

## License

This project is open source and available under the MIT License. 