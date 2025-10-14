import 'leaflet-element'

// Types for ambient globals
declare global {
    interface Window {
        testLogin: () => void;
    }
}

// Map placeholder to satisfy TypeScript when map features are disabled
let map: any | undefined;

// Ensure this file is treated as a module so global augmentation is valid
export {};

// Global variables for map functionality
// let map = null;
// let markers = [];

// Wait for DOMContentLoaded before setting up ports
exports.init = (app: any) => {
    // Wait for app to be ready before setting up ports
    console.log('🚀 Elm app initialized, setting up ports...');
    
    // Check if ports are available
    console.log('🔍 Available ports:', Object.keys(app.ports || {}));
    
    // Map functionality - wait for Elm to tell us to initialize
    app.ports.initMap.subscribe(function(config: any) {
        console.log('🗺️ Initializing map with config:', config);
        
        // Remove existing map if any
        // if (map) {
        //     map.remove();
        // }
        
        // Wait a bit for the DOM element to be available
        // setTimeout(() => {
        //     const mapElement = document.getElementById('map');
        //     if (!mapElement) {
        //         console.error('❌ Map element not found! Retrying...');
        //         // Retry after a longer delay
        //         setTimeout(() => {
        //             initializeMap(config);
        //         }, 500);
        //     } else {
        //         initializeMap(config);
        //     }
        // }, 100);
    });
    
    // function initializeMap(config) {
    //     const mapElement = document.getElementById('map');
    //     if (!mapElement) {
    //         console.error('❌ Map element still not found!');
    //         return;
    //     }
    //
    //     console.log('✅ Map element found, creating Leaflet map');
    //     map = L.map('map').setView([config.center.lat, config.center.lng], config.zoom);
    //
    //     L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
    //         attribution: '© OpenStreetMap contributors'
    //     }).addTo(map);
    //
    //     map.on('click', function(e) {
    //         app.ports.mapClicked.send({
    //             lat: e.latlng.lat,
    //             lng: e.latlng.lng
    //         });
    //     });
    //
    //     console.log('🗺️ Map initialized successfully!');
    // }
    
    // app.ports.setMapCenter.subscribe(function(config) {
    //     if (map) {
    //         map.setView([config.center.lat, config.center.lng], config.zoom);
    //     }
    // });
    
    // app.ports.addMarker.subscribe(function(data) {
    //     if (!map) return;
    //
    //     let iconClass, iconText;
    //
    //     switch(data.color) {
    //         case 'green':
    //             iconClass = 'ray-marker';
    //             iconText = '✈️';
    //             break;
    //         case 'blue':
    //             iconClass = 'guess-marker';
    //             iconText = '📍';
    //             break;
    //         case 'red':
    //             iconClass = 'other-guess-marker';
    //             iconText = '?';
    //             break;
    //         default:
    //             iconClass = 'guess-marker';
    //             iconText = '📍';
    //     }
    //
    //     const customIcon = L.divIcon({
    //         html: `<div class="${iconClass}">${iconText}</div>`,
    //         className: 'custom-div-icon',
    //         iconSize: [25, 25],
    //         iconAnchor: [12, 12]
    //     });
    //
    //     const marker = L.marker([data.location.lat, data.location.lng], {icon: customIcon})
    //         .addTo(map)
    //         .bindPopup(data.label);
    //
    //     markers.push(marker);
    // });
    
    // app.ports.clearMarkers.subscribe(function() {
    //     markers.forEach(marker => {
    //         map.removeLayer(marker);
    //     });
    //     markers = [];
    // });
    
    // Telegram Web Login functionality
    if (app.ports.authenticateWithTelegram) {
        console.log('✅ authenticateWithTelegram port found, subscribing...');
        app.ports.authenticateWithTelegram.subscribe(function() {
            console.log('🔍 authenticateWithTelegram port called!');
            
            // Check if running in Telegram Web App
            if (window.Telegram && window.Telegram.WebApp) {
                console.log('📱 Telegram Web App detected');
                const tg = window.Telegram.WebApp;
                tg.ready();
                
                if (tg.initDataUnsafe && tg.initDataUnsafe.user) {
                    console.log('👤 Telegram user data found:', tg.initDataUnsafe.user);
                    // Extract user data from Telegram Web App
                    const user = tg.initDataUnsafe.user;
                    const authData = JSON.stringify({
                        id: user.id,
                        first_name: user.first_name,
                        last_name: user.last_name,
                        username: user.username,
                        photo_url: user.photo_url
                    });
                    
                    console.log('📤 Sending Telegram auth data:', authData);
                    app.ports.telegramAuthResult.send(authData);
                } else {
                    console.error('❌ No user data available from Telegram');
                }
            } else {
                // Fallback for development/testing
                console.log('🧪 Telegram Web App not available, using test data');
                const testAuthData = JSON.stringify({
                    id: 123456789,
                    first_name: 'Test',
                    last_name: 'User',
                    username: 'testuser',
                    photo_url: null
                });
                
                console.log('📤 Sending test auth data:', testAuthData);
                
                try {
                    app.ports.telegramAuthResult.send(testAuthData);
                    console.log('✅ Test auth data sent successfully!');
                } catch (error) {
                    console.error('❌ Error sending test auth data:', error);
                }
            }
        });
    } else {
        console.error('❌ authenticateWithTelegram port not found!');
        console.log('Available ports:', Object.keys(app.ports || {}));
    }
    
    // Set up Telegram Web App styling
    if (window.Telegram && window.Telegram.WebApp) {
        const tg = window.Telegram.WebApp;
        tg.expand();
        tg.setHeaderColor('#2c3e50');
    }
    
    // Handle page visibility for map refresh
    document.addEventListener('visibilitychange', function() {
        if (!document.hidden && map) {
            setTimeout(() => {
                map.invalidateSize();
            }, 100);
        }
    });
    
    // Manual test function for debugging
    window.testLogin = function() {
        console.log('🔧 Manual test login triggered');
        const testAuthData = JSON.stringify({
            id: 123456789,
            first_name: 'Test',
            last_name: 'User',
            username: 'testuser',
            photo_url: null
        });
        
        console.log('📤 Manually sending test auth data:', testAuthData);
        try {
            app.ports.telegramAuthResult.send(testAuthData);
            console.log('✅ Manual test auth sent successfully!');
        } catch (error) {
            console.error('❌ Error in manual test auth:', error);
        }
    };
    
    // Fallback: Add direct event listener after page loads
    setTimeout(() => {
        const loginBtn = document.querySelector('.telegram-login-btn');
        if (loginBtn) {
            console.log('🔧 Adding fallback click listener to login button');
            loginBtn.addEventListener('click', function(e) {
                console.log('🔧 Fallback button clicked!');
                window.testLogin();
            });
        }
    }, 1000);
    
    // Make map globally accessible for debugging
    // window.rayMap = map;
}
