package org.kalypso.model.wspm.pdb.db.mapping;

import java.math.BigDecimal;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.Assert;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.model.wspm.pdb.db.constants.WaterBodyConstants;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.LineString;

/**
 * WaterBody generated by hbm2java
 */
@Entity
@Table(name = "water_body", schema = "pdb", uniqueConstraints = @UniqueConstraint(columnNames = "name"))
public class WaterBody extends AbstractModelObject implements java.io.Serializable, WaterBodyConstants
{
  private BigDecimal id;

  private String name = StringUtils.EMPTY;

  private Geometry riverline;

  private String label = StringUtils.EMPTY;

  private STATIONING_DIRECTION directionOfStationing = STATIONING_DIRECTION.upstream;

  private String description = StringUtils.EMPTY;

  private Integer rank;

  private Set<Event> events = new HashSet<Event>( 0 );

  private Set<CrossSection> crossSections = new HashSet<CrossSection>( 0 );

  private Set<Document> documents = new HashSet<Document>( 0 );

  public WaterBody( )
  {
  }

  public WaterBody( final BigDecimal id, final String name, final String label, final STATIONING_DIRECTION directionOfStationing )
  {
    this.id = id;
    this.name = name;
    this.label = label;
    this.directionOfStationing = directionOfStationing;
  }

  public WaterBody( final BigDecimal id, final String name, final Geometry riverline, final String label, final STATIONING_DIRECTION directionOfStationing, final Integer rank, final String description, final Set<Event> events, final Set<CrossSection> crossSections, final Set<Document> documents )
  {
    this.id = id;
    this.name = name;
    this.riverline = riverline;
    this.label = label;
    this.directionOfStationing = directionOfStationing;
    this.rank = rank;
    this.description = description;
    this.events = events;
    this.crossSections = crossSections;
    this.documents = documents;
  }

  @Id
  @Column(name = "id", unique = true, nullable = false, precision = 20, scale = 0)
  @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "waterbody_id_seq")
  @SequenceGenerator(name = "waterbody_id_seq", sequenceName = "pdb.seq_pdb")
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
  public Geometry getRiverline( )
  {
    return this.riverline;
  }

  public void setRiverline( final Geometry riverline )
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

  @Column(name = "direction_of_stationing", nullable = false, length = 20)
  @Enumerated(EnumType.STRING)
  public STATIONING_DIRECTION getDirectionOfStationing( )
  {
    return this.directionOfStationing;
  }

  public void setDirectionOfStationing( final STATIONING_DIRECTION directionOfStationing )
  {
    final Object oldValue = this.directionOfStationing;

    this.directionOfStationing = directionOfStationing;

    firePropertyChange( PROPERTY_DIRECTION_OF_STATIONING, oldValue, directionOfStationing );
  }

  @Column(name = "rank")
  public Integer getRank( )
  {
    return this.rank;
  }

  public void setRank( final Integer rank )
  {
    final Object oldValue = this.rank;

    this.rank = rank;

    firePropertyChange( PROPERTY_RANK, oldValue, rank );
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

  @OneToMany(fetch = FetchType.LAZY, mappedBy = "waterBody")
  public Set<Document> getDocuments( )
  {
    return this.documents;
  }

  public void setDocuments( final Set<Document> documents )
  {
    this.documents = documents;
  }

  @Transient
  public LineString getRiverlineAsLine( )
  {
    if( riverline instanceof LineString )
      return (LineString) riverline;

    if( riverline == null )
      return null;

    if( riverline instanceof GeometryCollection )
    {
      Assert.isTrue( riverline.isEmpty() );
      return null;
    }

    return null;
  }

  /**
   * Copy all (simple) properties of the given water body into this instance.
   */
  public void setAll( final WaterBody waterBody )
  {
    setDescription( waterBody.getDescription() );
    setDirectionOfStationing( waterBody.getDirectionOfStationing() );
    setLabel( waterBody.getLabel() );
    setName( waterBody.getName() );
    setRank( waterBody.getRank() );
    setRiverline( waterBody.getRiverline() );
  }
}