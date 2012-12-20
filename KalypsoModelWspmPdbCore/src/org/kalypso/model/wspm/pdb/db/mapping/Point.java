package org.kalypso.model.wspm.pdb.db.mapping;

import java.math.BigDecimal;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.JoinColumns;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

/**
 * Point generated by hbm2java
 */
@Entity
@Table(name = "point", schema = "pdb")
public class Point implements java.io.Serializable
{
  public static final String PROPERTY_WIDTH = "width"; //$NON-NLS-1$

  private BigDecimal m_id;

  private CrossSectionPart m_crossSectionPart;

  private Roughness m_roughness;

  private Vegetation m_vegetation;

  private String m_name;

  private com.vividsolutions.jts.geom.Point m_location;

  private long m_consecutiveNum;

  private String m_code;

  private String m_hyk;

  private BigDecimal m_width;

  private BigDecimal m_height;

  private BigDecimal m_roughnessKValue;

  private BigDecimal m_roughnessKstValue;

  private BigDecimal m_roughnessFactor;

  private BigDecimal m_vegetationDp;

  private BigDecimal m_vegetationAx;

  private BigDecimal m_vegetationAy;

  private String m_description;

  public Point( )
  {
  }

  public Point( final BigDecimal id, final CrossSectionPart crossSectionPart, final String name, final long consecutiveNum )
  {
    m_id = id;
    m_crossSectionPart = crossSectionPart;
    m_name = name;
    m_consecutiveNum = consecutiveNum;
  }

  public Point( final BigDecimal id, final CrossSectionPart crossSectionPart, final Roughness roughness, final Vegetation vegetation, final String name, final com.vividsolutions.jts.geom.Point location, final long consecutiveNum, final String code, final String hyk, final BigDecimal width, final BigDecimal height, final BigDecimal roughnessKValue, final BigDecimal roughnessKstValue, final BigDecimal vegetationDp, final BigDecimal vegetationAx, final BigDecimal vegetationAy, final String description, final BigDecimal roughnessFactor )
  {
    m_id = id;
    m_crossSectionPart = crossSectionPart;
    m_roughness = roughness;
    m_vegetation = vegetation;
    m_name = name;
    m_location = location;
    m_consecutiveNum = consecutiveNum;
    m_code = code;
    m_hyk = hyk;
    m_width = width;
    m_height = height;
    m_roughnessKValue = roughnessKValue;
    m_roughnessKstValue = roughnessKstValue;
    m_roughnessFactor = roughnessFactor;
    m_vegetationDp = vegetationDp;
    m_vegetationAx = vegetationAx;
    m_vegetationAy = vegetationAy;
    m_description = description;
  }

  @Id
  @Column(name = "id", unique = true, nullable = false, precision = 20, scale = 0)
  @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "point_id_seq")
  @SequenceGenerator(name = "point_id_seq", sequenceName = "pdb.seq_pdb")
  public BigDecimal getId( )
  {
    return m_id;
  }

  public void setId( final BigDecimal id )
  {
    m_id = id;
  }

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = "cross_section_part", nullable = false)
  public CrossSectionPart getCrossSectionPart( )
  {
    return m_crossSectionPart;
  }

  public void setCrossSectionPart( final CrossSectionPart crossSectionPart )
  {
    m_crossSectionPart = crossSectionPart;
  }

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumns({ @JoinColumn(name = "roughness_point_kind", referencedColumnName = "point_kind"), @JoinColumn(name = "roughness", referencedColumnName = "name") })
  public Roughness getRoughness( )
  {
    return m_roughness;
  }

  public void setRoughness( final Roughness roughness )
  {
    m_roughness = roughness;
  }

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumns({ @JoinColumn(name = "vegetation_point_kind", referencedColumnName = "point_kind"), @JoinColumn(name = "vegetation", referencedColumnName = "name") })
  public Vegetation getVegetation( )
  {
    return m_vegetation;
  }

  public void setVegetation( final Vegetation vegetation )
  {
    m_vegetation = vegetation;
  }

  @Column(name = "name", nullable = false, length = 50)
  public String getName( )
  {
    return m_name;
  }

  public void setName( final String name )
  {
    m_name = name;
  }

  @Column(name = "location", columnDefinition = "Geometry")
  public com.vividsolutions.jts.geom.Point getLocation( )
  {
    return m_location;
  }

  public void setLocation( final com.vividsolutions.jts.geom.Point location )
  {
    m_location = location;
  }

  @Column(name = "consecutive_num", nullable = false, precision = 11, scale = 0)
  public long getConsecutiveNum( )
  {
    return m_consecutiveNum;
  }

  public void setConsecutiveNum( final long consecutiveNum )
  {
    m_consecutiveNum = consecutiveNum;
  }

  @Column(name = "code", length = 50)
  public String getCode( )
  {
    return m_code;
  }

  public void setCode( final String code )
  {
    m_code = code;
  }

  @Column(name = "hyk", length = 50)
  public String getHyk( )
  {
    return m_hyk;
  }

  public void setHyk( final String hyk )
  {
    m_hyk = hyk;
  }

  @Column(name = "width", precision = 8, scale = 4)
  public BigDecimal getWidth( )
  {
    return m_width;
  }

  public void setWidth( final BigDecimal width )
  {
    m_width = width;
  }

  @Column(name = "height", precision = 8, scale = 4)
  public BigDecimal getHeight( )
  {
    return m_height;
  }

  public void setHeight( final BigDecimal height )
  {
    m_height = height;
  }

  @Column(name = "roughness_k_value", precision = 8, scale = 1)
  public BigDecimal getRoughnessKValue( )
  {
    return m_roughnessKValue;
  }

  public void setRoughnessKValue( final BigDecimal roughnessKValue )
  {
    m_roughnessKValue = roughnessKValue;
  }

  @Column(name = "roughness_kst_value", precision = 8, scale = 1)
  public BigDecimal getRoughnessKstValue( )
  {
    return m_roughnessKstValue;
  }

  public void setRoughnessKstValue( final BigDecimal roughnessKstValue )
  {
    m_roughnessKstValue = roughnessKstValue;
  }

  @Column(name = "roughness_factor", precision = 8, scale = 5)
  public BigDecimal getRoughnessFactor( )
  {
    return m_roughnessFactor;
  }

  public void setRoughnessFactor( final BigDecimal roughnessFactor )
  {
    m_roughnessFactor = roughnessFactor;
  }

  @Column(name = "vegetation_dp", precision = 8, scale = 3)
  public BigDecimal getVegetationDp( )
  {
    return m_vegetationDp;
  }

  public void setVegetationDp( final BigDecimal vegetationDp )
  {
    m_vegetationDp = vegetationDp;
  }

  @Column(name = "vegetation_ax", precision = 8, scale = 3)
  public BigDecimal getVegetationAx( )
  {
    return m_vegetationAx;
  }

  public void setVegetationAx( final BigDecimal vegetationAx )
  {
    m_vegetationAx = vegetationAx;
  }

  @Column(name = "vegetation_ay", precision = 8, scale = 3)
  public BigDecimal getVegetationAy( )
  {
    return m_vegetationAy;
  }

  public void setVegetationAy( final BigDecimal vegetationAy )
  {
    m_vegetationAy = vegetationAy;
  }

  @Column(name = "description")
  public String getDescription( )
  {
    return m_description;
  }

  public void setDescription( final String description )
  {
    m_description = description;
  }
}