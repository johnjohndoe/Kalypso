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

/**
 * @author Monika Thuel
 */
@Entity
@Table(name = "style_parameter", schema = "pdb")
public class StyleParameter implements Serializable
{
  private BigDecimal m_id;

  private String m_key;

  private String m_value;

  private Style m_style;

  public StyleParameter( )
  {
  }

  public StyleParameter( final BigDecimal id, final String key, final String value, final Style style )
  {
    m_id = id;
    m_key = key;
    m_value = value;
    m_style = style;
  }

  @Id
  @Column(name = "id", unique = true, nullable = false, precision = 20, scale = 0)
  @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "styleparam_id_seq")
  @SequenceGenerator(name = "styleparam_id_seq", sequenceName = "pdb.seq_pdb")
  public BigDecimal getId( )
  {
    return m_id;
  }

  public void setId( final BigDecimal id )
  {
    m_id = id;
  }

  @Column(name = "key", nullable = false, length = 50)
  public String getKey( )
  {
    return m_key;
  }

  public void setKey( final String key )
  {
    m_key = key;
  }

  @Column(name = "value", nullable = false, length = 255)
  public String getValue( )
  {
    return m_value;
  }

  public void setValue( final String value )
  {
    m_value = value;
  }

  @ManyToOne(fetch = FetchType.LAZY, targetEntity = Style.class)
  @JoinColumn(name = "style_id", nullable = false)
  public Style getStyle( )
  {
    return m_style;
  }

  public void setStyle( final Style style )
  {
    m_style = style;
  }

}
