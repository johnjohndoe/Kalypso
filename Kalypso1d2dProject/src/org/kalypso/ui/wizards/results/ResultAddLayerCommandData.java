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

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DHelper;
import org.kalypso.kalypsomodel1d2d.conv.results.NodeResultHelper;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;

/**
 *
 * Holds the data (except the GisTemplateMapModell), that is needed for the addThemeCommand.
 *
 * @author Thomas Jung
 */
public class ResultAddLayerCommandData
{
  private final Map<String, String> m_properties = new HashMap<>();

  private String m_themeName;

  private String m_resultType;

  private String m_featurePath;

  private String m_source;

  private String m_style;

  private String m_styleLocation;

  private IFile m_sldFile = null;

  private double m_minValue;

  private double m_maxValue;

  private boolean m_selected;

  private final IFolder m_scenarioFolder;

  private String m_type;

  private IDocumentResultMeta m_documentResult = null;

  public ResultAddLayerCommandData( final String themeName, final String resultType, final String featurePath, final String source, final String style, final String styleLocation, final IFolder scenarioFolder, final String type )
  {
    m_themeName = themeName;
    m_resultType = resultType;
    m_featurePath = featurePath;
    m_source = source;
    m_style = style;
    m_styleLocation = styleLocation;
    m_scenarioFolder = scenarioFolder;
    m_type = type;

    updateStyleLocation();
  }

  public void setSldFile( final IFile sldFile )
  {
    m_sldFile = sldFile;
    updateStyleLocation();
  }

  private void updateStyleLocation( )
  {
    final IFolder resultsFolder = KalypsoModel1D2DHelper.getResultsFolder( m_scenarioFolder );
    final String resFolder = resultsFolder.getFullPath().toPortableString();

    final String defaultPath = KalypsoModel1D2DHelper.getStylesFolder( m_scenarioFolder ).getFullPath().toPortableString();
    final String relativePathTo = FileUtilities.getRelativePathTo( resFolder, defaultPath );

    if( m_sldFile == null )
      m_styleLocation = ""; //$NON-NLS-1$
    else{
      final String sldFileName = m_sldFile.getName();
      if( sldFileName.toLowerCase().contains( NodeResultHelper.NODE_TYPE.toLowerCase() ) ){
        m_themeName = ResultMeta1d2dHelper.getNodeResultLayerName( m_themeName, sldFileName, NodeResultHelper.NODE_TYPE.toLowerCase() );
      }

      final IPath styleLocation = new Path( ".." ).append( relativePathTo ).append( m_type ).append( sldFileName ); //$NON-NLS-1$
      m_styleLocation = styleLocation.toPortableString();
    }
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

  public IFile getSldFile( )
  {
    return m_sldFile;
  }

  public void setValues( final String themeName, final String resultType, final String featurePath, final String source, final String style, final String styleLocation, final String type )
  {
    m_themeName = themeName;
    m_resultType = resultType;
    m_featurePath = featurePath;
    m_source = source;
    m_style = style;
    m_styleLocation = styleLocation;
    m_type = type;

    updateStyleLocation();
  }

  public double getMinValue( )
  {
    return m_minValue;
  }

  public void setMinValue( final double minValue )
  {
    m_minValue = minValue;
  }

  public double getMaxValue( )
  {
    return m_maxValue;
  }

  public void setMaxValue( final double maxValue )
  {
    m_maxValue = maxValue;
  }

  public boolean isSelected( )
  {
    return m_selected;
  }

  public void setSelected( final boolean selected )
  {
    m_selected = selected;
  }

  public void setProperty( final String name, final String value )
  {
    m_properties.put( name, value );
  }

  public Map<String, String> getProperties( )
  {
    return Collections.unmodifiableMap( m_properties );
  }

  public void setDocumentResult( final IDocumentResultMeta documentResult )
  {
    m_documentResult = documentResult;
  }

  public IDocumentResultMeta getDocumentResult( )
  {
    return m_documentResult;
  }

}
