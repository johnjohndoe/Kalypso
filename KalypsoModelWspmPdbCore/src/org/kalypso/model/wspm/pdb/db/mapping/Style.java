/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
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

/**
 * @author Monika Thuel
 */
@Entity
@Table( name = "style", schema = "pdb", uniqueConstraints = { @UniqueConstraint( columnNames = "name" ), @UniqueConstraint( columnNames = { "consecutive_num", "style_array_id" } ) } )
public class Style implements Serializable
{
  private BigDecimal m_id;

  private long m_consecutiveNum;

  private String m_name;

  private String m_description;

  private StyleArray m_styleArray;

  private Set<StyleParameter> m_styleParameters = new HashSet<>( 0 );

  public Style( )
  {
  }

  public Style( final BigDecimal id, final long consecutiveNum, final String name, final String description, final StyleArray styleArray )
  {
    m_id = id;
    m_consecutiveNum = consecutiveNum;
    m_name = name;
    m_description = description;
    m_styleArray = styleArray;
  }

  @Id
  @Column(name = "id", unique = true, nullable = false, precision = 20, scale = 0)
  @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "style_id_seq")
  @SequenceGenerator(name = "style_id_seq", sequenceName = "pdb.seq_pdb")
  public BigDecimal getId( )
  {
    return m_id;
  }

  public void setId( final BigDecimal id )
  {
    m_id = id;
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

  @Column(name = "name", nullable = false, length = 50)
  public String getName( )
  {
    return m_name;
  }

  public void setName( final String name )
  {
    m_name = name;
  }

  @Column(name = "description", length = 255)
  public String getDescription( )
  {
    return m_description;
  }

  public void setDescription( final String description )
  {
    m_description = description;
  }

  @ManyToOne(fetch = FetchType.LAZY, targetEntity = StyleArray.class)
  @JoinColumn(name = "style_array_id", nullable = false)
  public StyleArray getStyleArray( )
  {
    return m_styleArray;
  }

  public void setStyleArray( final StyleArray styleArray )
  {
    m_styleArray = styleArray;
  }

  @OneToMany(fetch = FetchType.LAZY, mappedBy = "style")
  public Set<StyleParameter> getStyleParameters( )
  {
    return m_styleParameters;
  }

  public void setStyleParameters( final Set<StyleParameter> styleParameters )
  {
    m_styleParameters = styleParameters;
  }

}
