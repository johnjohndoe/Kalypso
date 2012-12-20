package org.kalypso.model.wspm.pdb.db.mapping;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import org.kalypso.commons.java.util.AbstractModelObject;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;

/**
 * CrossSectionPart generated by hbm2java
 */
@Entity
@Table( name = "cross_section_part", schema = "pdb", uniqueConstraints = @UniqueConstraint( columnNames = { "name", "cross_section" } ) )
public class CrossSectionPart extends AbstractModelObject implements Serializable
{
  public static final String PROPERTY_DESCRIPTION = "description"; //$NON-NLS-1$

  private BigDecimal m_id;

  private CrossSection m_crossSection;

  private String m_name;

  private Geometry m_line;

  private String m_description;

  private Set<Point> m_points = new HashSet<>( 0 );

  private Event m_event;

  private Set<CrossSectionPartParameter> m_crossSectionPartParameters = new HashSet<>( 0 );

  private CrossSectionPartType m_crossSectionPartType;

  public CrossSectionPart( )
  {
  }

  public CrossSectionPart( final BigDecimal id, final CrossSection crossSection, final String name )
  {
    m_id = id;
    m_crossSection = crossSection;
    m_name = name;
  }

  public CrossSectionPart( final BigDecimal id, final CrossSection crossSection, final String name, final LineString line, final String description, final Set<Point> points )
  {
    m_id = id;
    m_crossSection = crossSection;
    m_name = name;
    m_line = line;
    m_description = description;
    m_points = points;
  }

  public CrossSectionPart( final BigDecimal id, final CrossSection crossSection, final String name, final LineString line, final String description, final Event event, final Set<Point> points, final Set<CrossSectionPartParameter> crossSectionPartParameters, final CrossSectionPartType crossSectionPartType )
  {
    m_id = id;
    m_crossSection = crossSection;
    m_name = name;
    m_line = line;
    m_description = description;
    m_event = event;
    m_points = points;
    m_crossSectionPartParameters = crossSectionPartParameters;
    m_crossSectionPartType = crossSectionPartType;
  }

  @Id
  @Column( name = "id", unique = true, nullable = false, precision = 20, scale = 0 )
  @GeneratedValue( strategy = GenerationType.SEQUENCE, generator = "cspart_id_seq" )
  @SequenceGenerator( name = "cspart_id_seq", sequenceName = "pdb.seq_pdb" )
  public BigDecimal getId( )
  {
    return m_id;
  }

  public void setId( final BigDecimal id )
  {
    m_id = id;
  }

  @ManyToOne( fetch = FetchType.LAZY )
  @JoinColumn( name = "cross_section", nullable = false )
  public CrossSection getCrossSection( )
  {
    return m_crossSection;
  }

  public void setCrossSection( final CrossSection crossSection )
  {
    m_crossSection = crossSection;
  }

  @Column( name = "name", nullable = false, length = 50 )
  public String getName( )
  {
    return m_name;
  }

  public void setName( final String name )
  {
    m_name = name;
  }

  @Column( name = "line", columnDefinition = "Geometry" )
  public Geometry getLine( )
  {
    return m_line;
  }

  public void setLine( final Geometry line )
  {
    m_line = line;

// if( line instanceof LineString )
// line = (LineString) line;
// else if( line == null )
// line = null;
// else if( line.isEmpty() )
// {
// // HACK: if geometry is empty convert to empty LineString so we always have a LineString
// // REMARK: this is needed, because we get an empty GeometryCollection from the db
// line = line.getFactory().createLineString( (Coordinate[]) null );
// }
// else
// throw new IllegalArgumentException( "'line' must be a LineString or Empty GeometryCollection" );
  }

  @ManyToOne( fetch = FetchType.LAZY, targetEntity = CrossSectionPartType.class )
  // @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn( name = "category", nullable = false )
  public CrossSectionPartType getCrossSectionPartType( )
  {
    return m_crossSectionPartType;
  }

  public void setCrossSectionPartType( final CrossSectionPartType crossSectionPartType )
  {
    m_crossSectionPartType = crossSectionPartType;
  }

  @Column( name = "description", length = 255 )
  public String getDescription( )
  {
    return m_description;
  }

  public void setDescription( final String description )
  {
    m_description = description;
  }

  @OneToMany( fetch = FetchType.LAZY, mappedBy = "crossSectionPart" )
  public Set<Point> getPoints( )
  {
    return m_points;
  }

  public void setPoints( final Set<Point> points )
  {
    m_points = points;
  }

  @ManyToOne( fetch = FetchType.LAZY )
  @JoinColumn( name = "event_id" )
  public Event getEvent( )
  {
    return m_event;
  }

  public void setEvent( final Event event )
  {
    m_event = event;
  }

  @OneToMany( fetch = FetchType.LAZY, mappedBy = "crossSectionPart" )
  public Set<CrossSectionPartParameter> getCrossSectionPartParameters( )
  {
    return m_crossSectionPartParameters;
  }

  public void setCrossSectionPartParameters( final Set<CrossSectionPartParameter> crossSectionPartParameters )
  {
    m_crossSectionPartParameters = crossSectionPartParameters;
  }
}