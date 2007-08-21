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
package org.kalypso.ui.wizards.results;

import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;

/**
 * 
 * Holds the data (except the GisTemplateMapModell), that is needed for the addThemeCommand.
 * 
 * @author Thomas Jung
 * 
 */
public class ResultAddLayerCommandData
{

  private final String m_themeName;

  private final String m_resultType;

  private final String m_featurePath;

  private final String m_source;

  private final String m_style;

  private final String m_styleLocation;

  private final IResultMeta m_calcUnitMeta;

  private final IResultMeta m_stepResultMeta;

  private final String m_styleLinkType;

  private final String m_styleType;

  public ResultAddLayerCommandData( String themeName, String resultType, String featurePath, String source, String style, String styleLocation, String styleLinkType, String styleType, IResultMeta calcUnitMeta, IResultMeta stepResultMeta )
  {
    m_themeName = themeName;
    m_resultType = resultType;
    m_featurePath = featurePath;
    m_source = source;
    m_style = style;
    m_styleLocation = styleLocation;
    m_styleLinkType = styleLinkType;
    m_styleType = styleType;
    m_calcUnitMeta = calcUnitMeta;
    m_stepResultMeta = stepResultMeta;

  }

  public String getThemeName( )
  {
    return m_themeName;
  }

  public String getResultType( )
  {
    return m_resultType;
  }

  public String getFeaturePath( )
  {
    return m_featurePath;
  }

  public String getSource( )
  {
    return m_source;
  }

  public String getStyle( )
  {
    return m_style;
  }

  public String getStyleLocation( )
  {
    return m_styleLocation;
  }

  public IResultMeta getCalcUnitMeta( )
  {
    return m_calcUnitMeta;
  }

  public IResultMeta getStepResultMeta( )
  {
    return m_stepResultMeta;
  }

  public String getStyleLinkType( )
  {
    return m_styleLinkType;
  }

  public String getStyleType( )
  {
    return m_styleType;
  }

}
