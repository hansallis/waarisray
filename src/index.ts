import 'leaflet-element'
import dayjs from 'dayjs'
import locale_nl from 'dayjs/locale/nl'
import calendar from 'dayjs/plugin/calendar'
import timezone from 'dayjs/plugin/timezone'
import utc from 'dayjs/plugin/utc'

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



dayjs.extend(calendar)
dayjs.extend(utc)
dayjs.extend(timezone)

class CalendarTime extends HTMLElement {
    set startWithLowerCase(value: boolean) {
        this._startWithLowerCase = value;
        this.updateContent();
    }
    set prefixIfPast(value: string) {
        this._prefixIfPast = value;
        this.updateContent();
    }
    set prefixIfFuture(value: string) {
        this._prefixIfFuture = value;
        this.updateContent();
    }
    private datetime: string;
    private _startWithLowerCase: boolean;
    private _prefixIfPast: string;
    private _prefixIfFuture: string;

    // connect component
    connectedCallback() {
        this.updateContent()
    }

    private lowercaseFirstLetter = (string : string) => {
        return string.charAt(0).toLowerCase() + string.slice(1);
    }

    updateContent() {
        let content = dayjs(this.datetime).tz("Europe/Amsterdam").locale("en").calendar(null, {
            sameDay: '[Today at] ' + locale_nl.formats.LT, // The same day ( Today at 14:30 )
            nextDay: '[Tomorrow at] ' + locale_nl.formats.LT, // The next day ( Tomorrow at 14:30 )
            nextWeek: 'dddd [at] ' + locale_nl.formats.LT, // The next week ( Sunday at 14:30 )
            lastDay: '[Yesterday at] ' + locale_nl.formats.LT, // The day before ( Yesterday at 14:30 )
            lastWeek: '[Last] dddd [om] ' + locale_nl.formats.LT, // Last week ( Last Monday at 14:30 )
            sameElse: locale_nl.formats.L + ' [at] ' + locale_nl.formats.LT // Everything else ( 17/10/2011 14:30 )
        });

        if (this._startWithLowerCase) {
            content = this.lowercaseFirstLetter(content);
        }
        if (dayjs(this.datetime).isBefore(dayjs())) {
            content = (this._prefixIfPast ?? '') + content;
        } else {
            content = (this._prefixIfFuture ?? '') + content;
        }
        this.textContent = content;
    }

    static get observedAttributes() {
        return ['datetime'];
    }

    constructor() {
        super();
        this.datetime = null;
        this._startWithLowerCase = false;
    }

    attributeChangedCallback(property: string, oldValue: any, newValue: string) {
        if (oldValue === newValue) return;
        switch (property) {
            case "datetime":
                this.datetime = newValue;
                this.updateContent();
        }


    }
}
if (undefined === window.customElements.get('calendar-time')) {
    customElements.define('calendar-time', CalendarTime);
}

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
    // app.ports.initMap.subscribe(function(config: any) {
    //     console.log('🗺️ Initializing map with config:', config);
        
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
    // });
    
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
                
                // Send the full initData string which includes the hash for verification
                if (tg.initData && tg.initData.length > 0) {
                    console.log('👤 Telegram initData found (length:', tg.initData.length, ')');
                    console.log('📤 Sending Telegram initData for verification');
                    app.ports.telegramAuthResult.send(tg.initData);
                } else {
                    console.error('❌ No initData available from Telegram');
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
    
    // Make map globally accessible for debugging
    // window.rayMap = map;
}
