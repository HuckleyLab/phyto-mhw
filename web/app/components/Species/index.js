/**
 *
 * Species
 *
 */

 import React from 'react';
 import { makeStyles } from '@material-ui/core/styles';
 import Chip from '@material-ui/core/Chip';
 import Card from '@material-ui/core/Card';
 import CardActions from '@material-ui/core/CardActions';
 import CardContent from '@material-ui/core/CardContent';
 import Button from '@material-ui/core/Button';
 import Typography from '@material-ui/core/Typography';
// import PropTypes from 'prop-types';
// import styled from 'styled-components';

const useStyles = makeStyles({
  card: {
    minWidth: '100%',
  },
  bullet: {
    display: 'inline-block',
    margin: '0 2px',
    transform: 'scale(0.8)',
  },
  title: {
    fontSize: 14,
  },
  pos: {
    marginBottom: 12,
  },
});

export default function Species(props) {
  const classes = useStyles();
  const bull = <span className={classes.bullet}>•</span>;
  var getChip = (tag, i) => {
    return (
      <Chip size="small" label={tag} color="secondary" key={tag}/>
    );
  }
  var bagOfChips = props.tags.map(getChip);

  return (
    <Card className={classes.card}>
      <CardContent>
        <Typography className={classes.title} color="textSecondary" gutterBottom>
          {props.sciName}
        </Typography>
        <Typography variant="h5" component="h2">
          {props.fish}
        </Typography>
        {bagOfChips}
        <Typography variant="body2" component="p">
          Days under stress: {props.stressAge}
        </Typography>
      </CardContent>
      <CardActions>
        <Button size="small">See Species distribution</Button>
      </CardActions>
    </Card>
  );
}
Species.propTypes = {};
