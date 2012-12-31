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

import java.math.BigDecimal;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.conv.results.NodeResultHelper;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.project.Scenario1D2D;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.ogc.gml.IKalypsoTheme;

/**
 * @author Thomas Jung
 * @author ilya
 */
public class NodeResultThemeCreator extends AbstractThemeCreator
{
  private final IDocumentResultMeta m_documentResult;

  private final IFolder m_scenarioFolder;

  private final ResultAddLayerCommandData m_resultLayerCommandData;

  private static final String LABEL_PROPERTY_FORMAT = String.format( "${property:%s#%s;-}", Kalypso1D2DSchemaConstants.TIN_RESULT_PROP_PARAMETER.getNamespaceURI(), Kalypso1D2DSchemaConstants.TIN_RESULT_PROP_PARAMETER.getLocalPart() ); //$NON-NLS-1$

  private static final String NODE_INFO_ID = "org.kalypso.kalypso1d2d.pjt.map.NodeThemeInfo"; //$NON-NLS-1$

  private static final String UNIT_PROPERTY_FORMAT = String.format( "${property:%s#%s;-}", Kalypso1D2DSchemaConstants.TIN_RESULT_PROP_UNIT.getNamespaceURI(), Kalypso1D2DSchemaConstants.TIN_RESULT_PROP_UNIT.getLocalPart() ); //$NON-NLS-1$

  private final String THEME_INFO_ID = String.format( "%s?geometry=%s&format=%s: %s %s", NODE_INFO_ID, Kalypso1D2DSchemaConstants.NODE_RESULT, LABEL_PROPERTY_FORMAT, "%.2f", UNIT_PROPERTY_FORMAT ); //$NON-NLS-1$ //$NON-NLS-2$

  private final BigDecimal m_minValue;

  private final BigDecimal m_maxValue;

  public NodeResultThemeCreator( final IDocumentResultMeta documentResult, final IFolder scenarioFolder )
  {
    m_documentResult = documentResult;
    m_minValue = m_documentResult.getMinValue();
    m_maxValue = m_documentResult.getMaxValue();
    m_scenarioFolder = scenarioFolder;

    m_resultLayerCommandData = updateThemeCommandData();
  }

  @Override
  public Composite createControl( final Group parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new GridLayout( 4, false ) );

    final Label nodeLabel = new Label( panel, SWT.FLAT );
    nodeLabel.setText( Messages.getString( "org.kalypso.ui.wizards.results.NodeResultThemeCreator.0" ) ); //$NON-NLS-1$

    /* create control with selection buttons, style combo-boxes, edit button and delete button */
    final ResultStyleComposite nodeStyleComp = new ResultStyleComposite( panel, m_scenarioFolder, NodeResultHelper.NODE_TYPE, m_minValue, m_maxValue, m_resultLayerCommandData );
    nodeStyleComp.getClass();

    return panel;
  }

  private ResultAddLayerCommandData updateThemeCommandData( )
  {
    final String featurePath = "nodeResultMember"; //$NON-NLS-1$

    final String source = TinResultThemeCreator.buildSourePath( m_documentResult, m_scenarioFolder );

    final String style = "Node Results Style"; //$NON-NLS-1$
    final String type = NodeResultHelper.NODE_TYPE;

    final String defaultStyleFileName = ResultMeta1d2dHelper.getDefaultStyleFileName( type, m_documentResult.getDocumentType().name() );

    // check, if there is a style already chosen, if not create one from default template
    final String styleLocation = getStyle( type, defaultStyleFileName );

    final ResultAddLayerCommandData resultLayerCommandData = new ResultAddLayerCommandData( featurePath, source, style, styleLocation, m_scenarioFolder, type, m_documentResult );

    resultLayerCommandData.setProperty( IKalypsoTheme.PROPERTY_DELETEABLE, Boolean.toString( true ) );
    resultLayerCommandData.setProperty( IKalypsoTheme.PROPERTY_THEME_INFO_ID, THEME_INFO_ID );
    resultLayerCommandData.setProperty( ResultAddLayerCommandData.PROPERTY_RESULT_TYPE, type );

    return resultLayerCommandData;
  }

  private String getStyle( final String type, final String defaultStyleFileName )
  {
    final Scenario1D2D scenario = new Scenario1D2D( m_scenarioFolder );
    final IFolder resultsFolder = scenario.getResultsFolder();
    final String resFolder = resultsFolder.getFullPath().toPortableString();

    final IFolder stylesFolder = scenario.getStylesFolder();
    final String defaultPath = stylesFolder.getFullPath().toPortableString();
    final String relativePathTo = FileUtilities.getRelativePathTo( resFolder, defaultPath );

    /* default location depending on type */
    final IFolder sldFolder = stylesFolder.getFolder( type );

    final String sldFileName = defaultStyleFileName;
    final IPath stylePath = new Path( ".." ).append( relativePathTo ).append( type ).append( sldFileName ); //$NON-NLS-1$
    final String styleLocation = stylePath.toPortableString();

    /* create sld file if it does not already exist */
    final IFile styleFile = sldFolder.getFile( sldFileName );
    if( !ResultSldHelper.allDefaultNodeStylesExist( sldFolder ) )
      ResultSldHelper.processStyle( styleFile, sldFolder, type, m_minValue, m_maxValue );

    return styleLocation;
  }

  @Override
  public ResultAddLayerCommandData[] getThemeCommandData( )
  {
    return new ResultAddLayerCommandData[] { m_resultLayerCommandData };
  }
}