package org.kalypso.model.wspm.pdb.db.mapping;

// default package
// Generated May 19, 2011 4:16:48 PM by Hibernate Tools 3.4.0.CR1

import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

/**
 * Events generated by hbm2java
 */
@Entity
@Table(name = "EVENTS", schema = "PDB_ADMIN")
public class Events implements java.io.Serializable
{

  private String name;

  private WaterBodies waterBodies;

  private Date creationDate;

  private Date editingDate;

  private String editingUser;

  private Date measurementDate;

  private String source;

  private String type;

  private String comment;

  private Set<WaterlevelFixations> waterlevelFixationses = new HashSet<WaterlevelFixations>( 0 );

  public Events( )
  {
  }

  public Events( final String name, final WaterBodies waterBodies, final Date creationDate, final Date editingDate, final String editingUser )
  {
    this.name = name;
    this.waterBodies = waterBodies;
    this.creationDate = creationDate;
    this.editingDate = editingDate;
    this.editingUser = editingUser;
  }

  public Events( final String name, final WaterBodies waterBodies, final Date creationDate, final Date editingDate, final String editingUser, final Date measurementDate, final String source, final String type, final String comment, final Set<WaterlevelFixations> waterlevelFixationses )
  {
    this.name = name;
    this.waterBodies = waterBodies;
    this.creationDate = creationDate;
    this.editingDate = editingDate;
    this.editingUser = editingUser;
    this.measurementDate = measurementDate;
    this.source = source;
    this.type = type;
    this.comment = comment;
    this.waterlevelFixationses = waterlevelFixationses;
  }

  @Id
  @Column(name = "NAME", unique = true, nullable = false, length = 100)
  public String getName( )
  {
    return this.name;
  }

  public void setName( final String name )
  {
    this.name = name;
  }

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = "WATER_BODY", nullable = false)
  public WaterBodies getWaterBodies( )
  {
    return this.waterBodies;
  }

  public void setWaterBodies( final WaterBodies waterBodies )
  {
    this.waterBodies = waterBodies;
  }

  @Temporal(TemporalType.DATE)
  @Column(name = "CREATION_DATE", nullable = false, length = 7)
  public Date getCreationDate( )
  {
    return this.creationDate;
  }

  public void setCreationDate( final Date creationDate )
  {
    this.creationDate = creationDate;
  }

  @Temporal(TemporalType.DATE)
  @Column(name = "EDITING_DATE", nullable = false, length = 7)
  public Date getEditingDate( )
  {
    return this.editingDate;
  }

  public void setEditingDate( final Date editingDate )
  {
    this.editingDate = editingDate;
  }

  @Column(name = "EDITING_USER", nullable = false, length = 50)
  public String getEditingUser( )
  {
    return this.editingUser;
  }

  public void setEditingUser( final String editingUser )
  {
    this.editingUser = editingUser;
  }

  @Temporal(TemporalType.DATE)
  @Column(name = "MEASUREMENT_DATE", length = 7)
  public Date getMeasurementDate( )
  {
    return this.measurementDate;
  }

  public void setMeasurementDate( final Date measurementDate )
  {
    this.measurementDate = measurementDate;
  }

  @Column(name = "SOURCE")
  public String getSource( )
  {
    return this.source;
  }

  public void setSource( final String source )
  {
    this.source = source;
  }

  @Column(name = "TYPE", length = 50)
  public String getType( )
  {
    return this.type;
  }

  public void setType( final String type )
  {
    this.type = type;
  }

  @Column(name = "description")
  public String getComment( )
  {
    return this.comment;
  }

  public void setComment( final String comment )
  {
    this.comment = comment;
  }

  @OneToMany(fetch = FetchType.LAZY, mappedBy = "events")
  public Set<WaterlevelFixations> getWaterlevelFixationses( )
  {
    return this.waterlevelFixationses;
  }

  public void setWaterlevelFixationses( final Set<WaterlevelFixations> waterlevelFixationses )
  {
    this.waterlevelFixationses = waterlevelFixationses;
  }

}
