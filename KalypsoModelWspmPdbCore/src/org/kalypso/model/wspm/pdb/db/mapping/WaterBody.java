package org.kalypso.model.wspm.pdb.db.mapping;

import java.math.BigDecimal;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import org.hibernate.annotations.Type;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.model.wspm.pdb.db.constants.WaterBodyConstants;

import com.vividsolutions.jts.geom.LineString;

/**
 * WaterBody generated by hbm2java
 */
@Entity
@Table(name = "water_body", schema = "pdb_admin", uniqueConstraints = @UniqueConstraint(columnNames = "name"))
public class WaterBody extends AbstractModelObject implements java.io.Serializable, WaterBodyConstants
{
  private BigDecimal id;

  private String name;

  private LineString riverline;

  private String label;

  private String description;

  private Set<Event> events = new HashSet<Event>( 0 );

  private Set<CrossSection> crossSections = new HashSet<CrossSection>( 0 );

  public WaterBody( )
  {
  }

  public WaterBody( final BigDecimal id, final String name, final String label )
  {
    this.id = id;
    this.name = name;
    this.label = label;
  }

  public WaterBody( final BigDecimal id, final String name, final LineString riverline, final String label, final String description, final Set<Event> events, final Set<CrossSection> crossSections )
  {
    this.id = id;
    this.name = name;
    this.riverline = riverline;
    this.label = label;
    this.description = description;
    this.events = events;
    this.crossSections = crossSections;
  }

  @Id
  @Column(name = "id", unique = true, nullable = false, precision = 20, scale = 0)
  @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "waterbody_id_seq")
  @SequenceGenerator(name = "waterbody_id_seq", sequenceName = "seq_pdb")
  public BigDecimal getId( )
  {
    return this.id;
  }

  public void setId( final BigDecimal id )
  {
    final Object oldValue = this.id;

    this.id = id;

    firePropertyChange( PROPERTY_ID, oldValue, id );
  }

  @Column(name = "name", unique = true, nullable = false, length = 100)
  public String getName( )
  {
    return this.name;
  }

  public void setName( final String name )
  {
    final Object oldValue = this.name;

    this.name = name;

    firePropertyChange( PROPERTY_NAME, oldValue, name );
  }

  @Column(name = "riverline", columnDefinition = "Geometry")
  @Type(type = "org.hibernatespatial.GeometryUserType")
  public LineString getRiverline( )
  {
    return this.riverline;
  }

  public void setRiverline( final LineString riverline )
  {
    final Object oldValue = this.riverline;

    this.riverline = riverline;

    firePropertyChange( PROPERTY_RIVERLINE, oldValue, riverline );
  }

  @Column(name = "label", nullable = false, length = 100)
  public String getLabel( )
  {
    return this.label;
  }

  public void setLabel( final String label )
  {
    final Object oldValue = this.label;

    this.label = label;

    firePropertyChange( PROPERTY_LABEL, oldValue, label );
  }

  @Column(name = "description")
  public String getDescription( )
  {
    return this.description;
  }

  public void setDescription( final String description )
  {
    final Object oldValue = this.description;

    this.description = description;

    firePropertyChange( PROPERTY_DESCRIPTION, oldValue, description );
  }

  @OneToMany(fetch = FetchType.LAZY, mappedBy = "waterBody")
  public Set<Event> getEvents( )
  {
    return this.events;
  }

  public void setEvents( final Set<Event> events )
  {
    this.events = events;
  }

  @OneToMany(fetch = FetchType.LAZY, mappedBy = "waterBody")
  public Set<CrossSection> getCrossSections( )
  {
    return this.crossSections;
  }

  public void setCrossSections( final Set<CrossSection> crossSections )
  {
    this.crossSections = crossSections;
  }
}