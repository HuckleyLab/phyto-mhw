/**
 *
 * SpeciesList
 *
 */

import React from 'react';
import Grid from '@material-ui/core/Grid';
import Species from 'components/Species';

class SpeciesList extends React.Component{
  constructor(props){
    super(props);
    this.state = {
      fish: "Brook Trout",
      sciName: "Salvelinus fontinalis",
      tags: ["Threatened"],
      stressAge: "15",
    };

  }

  render(){
    return (
      <Species fish={this.state.fish} sciName={this.state.sciName} tags={this.state.tags} stressAge={this.state.stressAge}/>
      );
  }
}

SpeciesList.propTypes = {};

export default SpeciesList;
