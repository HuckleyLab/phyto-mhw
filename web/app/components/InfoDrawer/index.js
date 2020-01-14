/**
 *
 * InfoDrawer
 *
 */
 import React from 'react';
 import clsx from 'clsx';
 import { makeStyles, useTheme } from '@material-ui/core/styles';
 import Drawer from '@material-ui/core/Drawer';
 import AppBar from '@material-ui/core/AppBar';
 import Toolbar from '@material-ui/core/Toolbar';
 import CssBaseline from '@material-ui/core/CssBaseline';
 import List from '@material-ui/core/List';
 import Typography from '@material-ui/core/Typography';
 import Divider from '@material-ui/core/Divider';
 import IconButton from '@material-ui/core/IconButton';
 import MenuIcon from '@material-ui/icons/Menu';
 import ChevronLeftIcon from '@material-ui/icons/ChevronLeft';
 import ChevronRightIcon from '@material-ui/icons/ChevronRight';
 import ListItem from '@material-ui/core/ListItem';
 import ListItemIcon from '@material-ui/core/ListItemIcon';
 import ListItemText from '@material-ui/core/ListItemText';
 import ListItemSecondaryAction from '@material-ui/core/ListItemSecondaryAction';
 import ArrowLeftIcon from '@material-ui/icons/ArrowLeft';
 import ListSubheader from '@material-ui/core/ListSubheader';
 import Collapse from '@material-ui/core/Collapse';
 import ExpandLess from '@material-ui/icons/ExpandLess';
 import ExpandMore from '@material-ui/icons/ExpandMore';
 import Switch from '@material-ui/core/Switch';
 import WifiIcon from '@material-ui/icons/Wifi';
 import BluetoothIcon from '@material-ui/icons/Bluetooth';
 import LayersIcon from '@material-ui/icons/Layers';

 const drawerWidth = 240;

 //Styles for drawer
 const useStyles = makeStyles(theme => ({
   root: {
     display: 'flex',
   },
   appBar: {
     transition: theme.transitions.create(['margin', 'width'], {
       easing: theme.transitions.easing.sharp,
       duration: theme.transitions.duration.leavingScreen,
     }),
   },
   appBarShift: {
     width: `calc(100% - ${drawerWidth}px)`,
     transition: theme.transitions.create(['margin', 'width'], {
       easing: theme.transitions.easing.easeOut,
       duration: theme.transitions.duration.enteringScreen,
     }),
     marginRight: drawerWidth,
   },
   title: {
     flexGrow: 1,
   },
   hide: {
     display: 'none',
   },
   drawer: {
     width: drawerWidth,
     flexShrink: 0,
   },
   drawerPaper: {
     width: drawerWidth,
   },
   drawerHeader: {
     display: 'flex',
     alignItems: 'center',
     padding: theme.spacing(0, 1),
     ...theme.mixins.toolbar,
     justifyContent: 'flex-start',
   },
   content: {
     flexGrow: 1,
     padding: theme.spacing(3),
     transition: theme.transitions.create('margin', {
       easing: theme.transitions.easing.sharp,
       duration: theme.transitions.duration.leavingScreen,
     }),
     marginRight: -drawerWidth,
   },
   contentShift: {
     transition: theme.transitions.create('margin', {
       easing: theme.transitions.easing.easeOut,
       duration: theme.transitions.duration.enteringScreen,
     }),
     marginRight: 0,
   },
 }));

 export default function InfoDrawer(props) {
   const classes = useStyles();
   const theme = useTheme();
   const [open, setOpen] = React.useState(false);

   //Handle opening and closing drawers
   const handleDrawerOpen = () => {
     setOpen(true);
   };
   const handleDrawerClose = () => {
     setOpen(false);
   };

   //Handle opening and closing nested list
  const [openInfo, setOpenInfo] = React.useState(false);
  const handleClick = () => {
    setOpenInfo(!openInfo);
  };

  //Handles layer toggles
  const [checked, setChecked] = React.useState(['seafloor']);
  const handleToggle = value => () => {
    const currentIndex = checked.indexOf(value);
    const newChecked = [...checked];

    if (currentIndex === -1) {
      newChecked.push(value);
    } else {
      newChecked.splice(currentIndex, 1);
    }

    setChecked(newChecked);
  };

   return (
     <div className={classes.root}>
       <CssBaseline />
           <IconButton
             // style={{height: "100%"}}
             color="inherit"
             aria-label="open drawer"
             edge="end"
             onClick={handleDrawerOpen}
             className={clsx(open && classes.hide)}
           >
             <ChevronLeftIcon fontSize="large" style={{align:'right'}}/>
           </IconButton>

       <main
         className={clsx(classes.content, {
           [classes.contentShift]: open,
         })}
       >
         <div className={classes.drawerHeader} />
       </main>

       <Drawer
         className={classes.drawer}
         variant="persistent"
         anchor="right"
         open={open}
         classes={{
           paper: classes.drawerPaper,
         }}
       >
         <div className={classes.drawerHeader}>
           <IconButton onClick={handleDrawerClose}>
             {theme.direction === 'rtl' ? <ChevronLeftIcon /> : <ChevronRightIcon />}
           </IconButton>
         </div>
         <Divider />
         <List>
           <ListItem >
             <p> Latitude: {props.lat} </p>
           </ListItem>
           <ListItem >
             <p> Longitude: {props.lon} </p>
           </ListItem>
           <ListItem >
             <p> Zoom: {props.zm} </p>
           </ListItem>

           <Divider />

           <ListItem button onClick={handleClick}>
            <ListItemIcon>
              <LayersIcon />
            </ListItemIcon>
            <ListItemText primary="Toggle Layers" />
              {openInfo ? <ExpandLess /> : <ExpandMore />}
           </ListItem>
           <Collapse in={openInfo} timeout="auto" unmountOnExit>
             <List component="div" disablePadding>
               <ListItem>
                <ListItemText id="switch-list-label-seafloor" primary="Seafloor" />
                <ListItemSecondaryAction>
                  <Switch
                    edge="end"
                    onChange={handleToggle('seafloor')}
                    checked={checked.indexOf('seafloor') !== -1}
                    inputProps={{ 'aria-labelledby': 'switch-list-label-seafloor' }}
                  />
                </ListItemSecondaryAction>
              </ListItem>

              <ListItem>
               <ListItemText id="switch-list-label-ecozone" primary="Economic Zones" />
               <ListItemSecondaryAction>
                 <Switch
                   edge="end"
                   onChange={handleToggle('ecozone')}
                   checked={checked.indexOf('ecozone') !== -1}
                   inputProps={{ 'aria-labelledby': 'switch-list-label-ecozone' }}
                 />
               </ListItemSecondaryAction>
             </ListItem>

              <ListItem>
               <ListItemText id="switch-list-label-currents" primary="Currents" />
               <ListItemSecondaryAction>
                 <Switch
                   edge="end"
                   onChange={handleToggle('currents')}
                   checked={checked.indexOf('currents') !== -1}
                   inputProps={{ 'aria-labelledby': 'switch-list-label-currents' }}
                 />
               </ListItemSecondaryAction>
             </ListItem>

             <ListItem>
              <ListItemText id="switch-list-label-wind" primary="Wind" />
              <ListItemSecondaryAction>
                <Switch
                  edge="end"
                  onChange={handleToggle('wind')}
                  checked={checked.indexOf('wind') !== -1}
                  inputProps={{ 'aria-labelledby': 'switch-list-label-wind' }}
                />
              </ListItemSecondaryAction>
            </ListItem>
           </List>
          </Collapse>
         </List>
        <Divider />
       </Drawer>
     </div>
   );
 }
