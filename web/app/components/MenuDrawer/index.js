/**
 *
 * MenuDrawer
 *
 */
import React from 'react';
import { makeStyles } from '@material-ui/core/styles';
import Drawer from '@material-ui/core/Drawer';
import Button from '@material-ui/core/Button';
import List from '@material-ui/core/List';
import Divider from '@material-ui/core/Divider';
import ListItem from '@material-ui/core/ListItem';
import ListItemIcon from '@material-ui/core/ListItemIcon';
import ListItemText from '@material-ui/core/ListItemText';
import InboxIcon from '@material-ui/icons/MoveToInbox';
import MailIcon from '@material-ui/icons/Mail';
import MenuIcon from '@material-ui/icons/Menu';
import InfoTwoToneIcon from '@material-ui/icons/InfoTwoTone';
import PeopleTwoToneIcon from '@material-ui/icons/PeopleTwoTone';

const useStyles = makeStyles({
  list: {
    width: 250,
  },
  fullList: {
    width: 'auto',
  },
});

export default function MenuDrawer() {
  const classes = useStyles();
  const [state, setState] = React.useState({
    top: false,
    left: false,
    bottom: false,
    right: false,
  });

  const toggleDrawer = (side, open) => event => {
    if (event.type === 'keydown' && (event.key === 'Tab' || event.key === 'Shift')) {
      return;
    }

    setState({ ...state, [side]: open });
  };

  const sideList = side => (
    <div
      className={classes.list}
      role="presentation"
      onClick={toggleDrawer(side, false)}
      onKeyDown={toggleDrawer(side, false)}
    >
      <List>
        <a href="http://faculty.washington.edu/lbuckley/" target="_blank">
        <ListItem button key='Our lab' >
          <ListItemIcon><PeopleTwoToneIcon /> </ListItemIcon>
          <ListItemText primary={'About us'} />
        </ListItem>
        </a>

        <a href="https://github.com/HuckleyLab/mhw_stressviz" target="_blank">
        <ListItem button key='About the project' >
          <ListItemIcon><InfoTwoToneIcon /> </ListItemIcon>
          <ListItemText primary={'About the project'} />
        </ListItem>
        </a>
      </List>
      <Divider />

    </div>
  );



  return (
    <div >
      <Button onClick={toggleDrawer('left', true)} ><MenuIcon /></Button>

      <Drawer open={state.left} onClose={toggleDrawer('left', false)}>
        {sideList('left')}
      </Drawer>

    </div>
  );
}


MenuDrawer.propTypes = {};
