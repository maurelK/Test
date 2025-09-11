// API configuration for connecting to Django backend
const API_BASE_URL = 'http://localhost:8000/JEB';

export const api = {
  // Base URL for API calls
  baseURL: API_BASE_URL,

  // Endpoints
  endpoints: {
    events: `${API_BASE_URL}/events`,
    news: `${API_BASE_URL}/news`,
    startups: `${API_BASE_URL}/startup`,
    investors: `${API_BASE_URL}/investors`,
    founders: `${API_BASE_URL}/founder`,
    partners: `${API_BASE_URL}/partner`,
    users: `${API_BASE_URL}/user`,
  },

  // Generic fetch function
  async fetchData(endpoint, options = {}) {
    const url = endpoint.startsWith('http') ? endpoint : `${this.baseURL}${endpoint}`;

    const config = {
      headers: {
        'Content-Type': 'application/json',
        ...options.headers,
      },
      ...options,
    };

    try {
      const response = await fetch(url, config);
      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }
      return await response.json();
    } catch (error) {
      console.error('API fetch error:', error);
      throw error;
    }
  },

  // Specific methods for different resources
  async getStartups() {
    return this.fetchData('/startup/');
  },

  async getEvents() {
    return this.fetchData('/events/');
  },

  async getNews() {
    return this.fetchData('/news/');
  },

  async getInvestors() {
    return this.fetchData('/investors/');
  },

  async getFounders() {
    return this.fetchData('/founder/');
  },

  async getPartners() {
    return this.fetchData('/partner/');
  },

  async getUsers() {
    return this.fetchData('/user/');
  },
};

export default api;
