/**
 *
 * Map
 *
 */

import React from 'react';
import mapboxgl from 'mapbox-gl';
import styled from 'styled-components';
mapboxgl.accessToken = 'pk.eyJ1IjoiaWNhcnVzbzIxIiwiYSI6ImNrNDljY2x6bzAyZDQza24ydmthcjVnNzgifQ.H_cs4gMzlO8sdRZ2hwSJ8w';
import data from './data.json';
import 'mapbox-gl/dist/mapbox-gl.css';

const MapContainer = styled.div`
  top: 0;
  bottom: 0;
  height: 100%;
  width: 100%;
  `;

  const countries = styled.div`
    position: absolute;
    `;

  //Input for layer paint toggle
  const options = [{
    name: 'Sea Surface Temperature',
    description: 'Estimated total population',
    property: 'pop_est',
    stops: [
      [0, '#f8d5cc'],
      [1000000, '#f4bfb6'],
      [5000000, '#f1a8a5'],
      [10000000, '#ee8f9a'],
      [50000000, '#ec739b'],
      [100000000, '#dd5ca8'],
      [250000000, '#c44cc0'],
      [500000000, '#9f43d7'],
      [1000000000, '#6e40e6']
    ]
  }, {
    name: 'Thermal Stress',
    description: 'Estimate total GDP in millions of dollars',
    property: 'gdp_md_est',
    stops: [
      [0, '#f8d5cc'],
      [1000, '#f4bfb6'],
      [5000, '#f1a8a5'],
      [10000, '#ee8f9a'],
      [50000, '#ec739b'],
      [100000, '#dd5ca8'],
      [250000, '#c44cc0'],
      [5000000, '#9f43d7'],
      [10000000, '#6e40e6']
    ]
  }]

class Map extends React.Component{
  constructor(props) {
    super(props);
    this.state = {
      lat: 0,
      lng: 0,
      zoom: 0,
      active: options[0]
    }
  }

  componentDidUpdate(){
    this.setFill();
  }

  componentDidMount() {
    this.map = new mapboxgl.Map({
      container: 'map',
      //style: 'mapbox://styles/icaruso21/ck51mnpzd0ela1cqfcqr6temc',
      style: 'mapbox://styles/mapbox/streets-v9',
    });

    //Load and add 'countries' layer
    this.map.on('load', () => {
      this.map.addSource('countries', {
        type: 'geojson',
        data
      });
      this.map.addLayer({
        id: 'countries',
        type: 'fill',
        source: 'countries'
      }, 'country-label-lg');
      this.setFill();
    });

    //Add zoom & compass buttons to the map
    var nav = new mapboxgl.NavigationControl();
    this.map.addControl(nav, 'bottom-right');

    //Handles updating pointer location and current zoom level
    this.map.on('mousemove',(e) => {
      console.log(e.lngLat.wrap());
      this.setState({
        lng: e.lngLat.wrap().lng,
        lat: e.lngLat.wrap().lat,
        zoom: this.map.getZoom()
      });
      this.props.mapMoveHandler({
        latitude: this.state.lat,
        longitude: this.state.lng,
        zoom: this.state.zoom
      });
    });
    this.map.on('zoomend',(e) => {
      this.setState({zoom: this.map.getZoom()});
      this.props.mapMoveHandler({
        latitude: this.state.lat,
        longitude: this.state.lng,
        zoom: this.state.zoom
      });
    });
  }

  //Sets fill for 'countries' layer
  setFill() {
      const { property, stops } = this.state.active;
      this.map.setPaintProperty('countries', 'fill-color', {
        property,
        stops
      });
    }

  render(){

    //consts for layer paint toggle
    const { name, description, stops, property } = this.state.active;
    const renderLegendKeys = (stop, i) => {
      return (
        <div key={i} className='txt-s'>
          <span className='mr6 round-full w12 h12 inline-block align-middle' style={{ backgroundColor: stop[1] }} />
          <span>{`${stop[0].toLocaleString()}`}</span>
        </div>
      );
    }

    const renderOptions = (option, i) => {
      return (
        <label key={i} className="toggle-container">
          <input onChange={() => this.setState({ active: options[i] })} checked={option.property === property} name="toggle" type="radio" />
          <div className="toggle txt-s py3 toggle--active-white">{option.name}</div>
        </label>
      );
    }

    return(
        <MapContainer id="map">
        <div ref={el => this.mapContainer = el} className="absolute top right left bottom" />
          <div className="toggle-group absolute bottom left ml12 mb30 border border--2 border--white bg-white shadow-darken10 z1">
            {options.map(renderOptions)}
          </div>
          <div className="bg-white absolute bottom right mr12 mb24 py12 px12 shadow-darken10 round z1 wmax180">
            <div className='mb6'>
              <h2 className="txt-bold txt-s block">{name}</h2>
              <p className='txt-s color-gray'>{description}</p>
            </div>
            {stops.map(renderLegendKeys)}
          </div>
        </MapContainer>
    )
  }
}

export default Map;
