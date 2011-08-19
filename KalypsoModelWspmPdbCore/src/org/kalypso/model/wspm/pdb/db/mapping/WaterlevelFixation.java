package org.kalypso.model.wspm.pdb.db.mapping;

import java.math.BigDecimal;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import org.apache.commons.lang3.StringUtils;

/**
 * WaterlevelFixation generated by hbm2java
 */
@Entity
@Table(name = "waterlevel_fixation", schema = "pdb")
public class WaterlevelFixation implements java.io.Serializable
{
  public static final String PROPERTY_STATION = "station"; //$NON-NLS-1$

  public static final String PROPERTY_WATERLEVEL = "waterlevel"; //$NON-NLS-1$

  public static final String PROPERTY_DISCHARGE = "discharge"; //$NON-NLS-1$

  public static final String PROPERTY_MEASURMENT_DATE = "measurementDate"; //$NON-NLS-1$

  public static final String PROPERTY_DESCRIPTION = "description"; //$NON-NLS-1$

  private BigDecimal id;

  private Event event;

  private BigDecimal station;

  private com.vividsolutions.jts.geom.Point location;

  private Date creationDate;

  private Date editingDate;

  private String editingUser;

  private Date measurementDate;

  private BigDecimal waterlevel;

  private BigDecimal discharge;

  private String description = StringUtils.EMPTY;

  public WaterlevelFixation( )
  {
  }

  public WaterlevelFixation( final BigDecimal id, final Event event, final BigDecimal station, final Date creationDate, final Date editingDate, final String editingUser )
  {
    this.id = id;
    this.event = event;
    this.station = station;
    this.creationDate = creationDate;
    this.editingDate = editingDate;
    this.editingUser = editingUser;
  }

  public WaterlevelFixation( final BigDecimal id, final Event event, final BigDecimal station, final com.vividsolutions.jts.geom.Point location, final Date creationDate, final Date editingDate, final String editingUser, final Date measurementDate, final BigDecimal waterlevel, final BigDecimal discharge, final String description )
  {
    this.id = id;
    this.event = event;
    this.station = station;
    this.location = location;
    this.creationDate = creationDate;
    this.editingDate = editingDate;
    this.editingUser = editingUser;
    this.measurementDate = measurementDate;
    this.waterlevel = waterlevel;
    this.discharge = discharge;
    this.description = description;
  }

  @Id
  @Column(name = "id", unique = true, nullable = false, precision = 20, scale = 0)
  @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "waterlevel_fixation_id_seq")
  @SequenceGenerator(name = "waterlevel_fixation_id_seq", sequenceName = "pdb.seq_pdb")
  public BigDecimal getId( )
  {
    return this.id;
  }

  public void setId( final BigDecimal id )
  {
    this.id = id;
  }

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = "event", nullable = false)
  public Event getEvent( )
  {
    return this.event;
  }

  public void setEvent( final Event event )
  {
    this.event = event;
  }

  @Column(name = "station", nullable = false, precision = 8, scale = 1)
  public BigDecimal getStation( )
  {
    return this.station;
  }

  public void setStation( final BigDecimal station )
  {
    this.station = station;
  }

  @Column(name = "location", columnDefinition = "Geometry")
  public com.vividsolutions.jts.geom.Point getLocation( )
  {
    return this.location;
  }

  public void setLocation( final com.vividsolutions.jts.geom.Point location )
  {
    this.location = location;
  }

  @Temporal(TemporalType.TIMESTAMP)
  @Column(name = "creation_date", nullable = false, length = 22)
  public Date getCreationDate( )
  {
    return this.creationDate;
  }

  public void setCreationDate( final Date creationDate )
  {
    this.creationDate = creationDate;
  }

  @Temporal(TemporalType.TIMESTAMP)
  @Column(name = "editing_date", nullable = false, length = 22)
  public Date getEditingDate( )
  {
    return this.editingDate;
  }

  public void setEditingDate( final Date editingDate )
  {
    this.editingDate = editingDate;
  }

  @Column(name = "editing_user", nullable = false, length = 50)
  public String getEditingUser( )
  {
    return this.editingUser;
  }

  public void setEditingUser( final String editingUser )
  {
    this.editingUser = editingUser;
  }

  @Temporal(TemporalType.TIMESTAMP)
  @Column(name = "measurement_date", length = 22)
  public Date getMeasurementDate( )
  {
    return this.measurementDate;
  }

  public void setMeasurementDate( final Date measurementDate )
  {
    this.measurementDate = measurementDate;
  }

  @Column(name = "waterlevel", precision = 8, scale = 3)
  public BigDecimal getWaterlevel( )
  {
    return this.waterlevel;
  }

  public void setWaterlevel( final BigDecimal waterlevel )
  {
    this.waterlevel = waterlevel;
  }

  @Column(name = "discharge", precision = 8, scale = 3)
  public BigDecimal getDischarge( )
  {
    return this.discharge;
  }

  public void setDischarge( final BigDecimal discharge )
  {
    this.discharge = discharge;
  }

  @Column(name = "description")
  public String getDescription( )
  {
    return this.description;
  }

  public void setDescription( final String description )
  {
    this.description = description;
  }

}
