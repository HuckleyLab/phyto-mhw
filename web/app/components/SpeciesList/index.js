/**
 *
 * SpeciesList
 *
 */

import React from 'react';
import Grid from '@material-ui/core/Grid';
import Species from 'components/Species';
import List from '@material-ui/core/List';
import ListItem from '@material-ui/core/ListItem';
import ListItemIcon from '@material-ui/core/ListItemIcon';
import ListItemText from '@material-ui/core/ListItemText';
import Divider from '@material-ui/core/Divider';
import InboxIcon from '@material-ui/icons/Inbox';
import DraftsIcon from '@material-ui/icons/Drafts';

function ListItemLink(props) {
  return <ListItem button component="a" {...props} />;
}

class SpeciesList extends React.Component{
  constructor(props){
    super(props);
    this.state = {
      fish: "Redband Trout",
      sciName: "Salvelinus fontinalis",
      tags: ["Threatened"],
      stressAge: "15",
    };


  }

  render(){
    return (
        <List component="nav" aria-label="secondary mailbox folders" style={{width:'100%',maxHeight:'100vh',overflow:'auto'}}>
          <ListItem >
            <Species fish={this.state.fish} sciName={this.state.sciName} tags={this.state.tags} stressAge={this.state.stressAge}/>
          </ListItem>
          <ListItem >
            <Species fish={this.state.fish} sciName={this.state.sciName} tags={this.state.tags} stressAge={this.state.stressAge}/>
          </ListItem>
          <ListItem >
            <Species fish={this.state.fish} sciName={this.state.sciName} tags={this.state.tags} stressAge={this.state.stressAge}/>
          </ListItem>
          <ListItem >
            <Species fish={this.state.fish} sciName={this.state.sciName} tags={this.state.tags} stressAge={this.state.stressAge}/>
          </ListItem>
          <ListItem >
            <Species fish={this.state.fish} sciName={this.state.sciName} tags={this.state.tags} stressAge={this.state.stressAge}/>
          </ListItem>
        </List>
    );
  }
}

SpeciesList.propTypes = {};

export default SpeciesList;
