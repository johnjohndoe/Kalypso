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
import javax.persistence.OneToMany;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

/**
 * @author Monika Thuel
 */
@Entity
@Table(name = "style_array", schema = "pdb")
public class StyleArray implements Serializable
{
  private BigDecimal m_id;

  private String m_name;

  private Set<CrossSectionPartType> m_crossSectionPartTypes = new HashSet<>( 0 );

  private Set<Style> m_styles = new HashSet<>( 0 );

  public StyleArray( )
  {
  }

  public StyleArray( final BigDecimal id, final String name )
  {
    m_id = id;
    m_name = name;
  }

  @Id
  @Column(name = "id", unique = true, nullable = false, precision = 20, scale = 0)
  @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "stylearray_id_seq")
  @SequenceGenerator(name = "stylearray_id_seq", sequenceName = "pdb.seq_pdb")
  public BigDecimal getId( )
  {
    return m_id;
  }

  public void setId( final BigDecimal id )
  {
    m_id = id;
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

// @OneToOne(fetch = FetchType.LAZY)
// @JoinColumn(name = "id", nullable = false)
// public CrossSectionPartType getCrossSectionPartType( )
// {
// return m_crossSectionPartType;
// }
//
// public void setCrossSectionPartTyp( final CrossSectionPartType crossSectionPartTyp )
// {
// m_crossSectionPartType = crossSectionPartTyp;
// }

  @OneToMany(fetch = FetchType.LAZY, mappedBy = "styleArray")
  public Set<CrossSectionPartType> getCrossSectionPartTypes( )
  {
    return m_crossSectionPartTypes;
  }

  public void setCrossSectionPartTypes( final Set<CrossSectionPartType> crossSectionPartTypes )
  {
    m_crossSectionPartTypes = crossSectionPartTypes;
  }

  @OneToMany(fetch = FetchType.LAZY, mappedBy = "styleArray")
  public Set<Style> getStyles( )
  {
    return m_styles;
  }

  public void setStyles( final Set<Style> styles )
  {
    m_styles = styles;
  }

}
