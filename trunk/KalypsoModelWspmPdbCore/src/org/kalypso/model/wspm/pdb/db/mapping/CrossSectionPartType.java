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

import javax.persistence.Column;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;

import org.kalypso.model.wspm.pdb.db.constants.CategoryConstants;

/**
 * @author Monika Thuel
 */
public class CrossSectionPartType implements Serializable, CategoryConstants
{
  private CATEGORY m_category;

  private String m_description;

  private StyleArray m_styleArray;

  public CrossSectionPartType( )
  {
  }

  public CrossSectionPartType( final CATEGORY category, final String description, final StyleArray styleArray )
  {
    m_category = category;
    m_description = description;
    m_styleArray = styleArray;
  }

  @Id
  @Column(name = "category", unique = true, nullable = false, length = 50)
  @Enumerated(EnumType.STRING)
  public CATEGORY getCategory( )
  {
    return m_category;
  }

  public void setCategory( final CATEGORY category )
  {
    m_category = category;
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

  @OneToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = "style_array_id", nullable = false)
  public StyleArray getStyleArray( )
  {
    return m_styleArray;
  }

  public void setStyleArray( final StyleArray styleArray )
  {
    m_styleArray = styleArray;
  }
}
