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

import java.io.File;
import java.math.BigDecimal;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DHelper;
import org.kalypso.kalypsomodel1d2d.conv.results.NodeResultHelper;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ui.wizards.i18n.Messages;

/**
 * @author Thomas Jung
 * @author ilya
 * 
 */
public class NodeResultThemeCreator extends AbstractThemeCreator
{

  private final IDocumentResultMeta m_documentResult;

  private final IFolder m_scenarioFolder;

  private final ResultAddLayerCommandData[] m_resultLayerCommandData = new ResultAddLayerCommandData[1];

  private static final String LABEL_PROPERTY_FORMAT = String.format( "${property:%s#%s;-}", Kalypso1D2DSchemaConstants.TIN_RESULT_PROP_PARAMETER.getNamespaceURI(), Kalypso1D2DSchemaConstants.TIN_RESULT_PROP_PARAMETER.getLocalPart() ); //$NON-NLS-1$

  private static final String NODE_INFO_ID = "org.kalypso.kalypso1d2d.pjt.map.NodeThemeInfo"; //$NON-NLS-1$

  private static final String UNIT_PROPERTY_FORMAT = String.format( "${property:%s#%s;-}", Kalypso1D2DSchemaConstants.TIN_RESULT_PROP_UNIT.getNamespaceURI(), Kalypso1D2DSchemaConstants.TIN_RESULT_PROP_UNIT.getLocalPart() ); //$NON-NLS-1$

  private final String THEME_INFO_ID = String.format( "%s?geometry=%s&format=%s: %s %s", NODE_INFO_ID, Kalypso1D2DSchemaConstants.NODE_RESULT, LABEL_PROPERTY_FORMAT, "%.2f", UNIT_PROPERTY_FORMAT ); //$NON-NLS-1$ //$NON-NLS-2$

  private ResultStyleComposite m_nodeStyleComp;

  private final BigDecimal m_minValue;

  private final BigDecimal m_maxValue;

  public NodeResultThemeCreator( final IDocumentResultMeta documentResult, final IFolder scenarioFolder )
  {
    m_documentResult = documentResult;
    m_minValue = m_documentResult.getMinValue();
    m_maxValue = m_documentResult.getMaxValue();
    m_scenarioFolder = scenarioFolder;

    updateThemeCommandData();
  }

  @Override
  public Composite createControl( final Group parent )
  {
    final Composite buttonComp = new Composite( parent, SWT.NONE );
    buttonComp.setLayout( new GridLayout( 4, false ) );

    final Label nodeLabel = new Label( buttonComp, SWT.FLAT );
    nodeLabel.setText( Messages.getString("org.kalypso.ui.wizards.results.NodeResultThemeCreator.0") ); //$NON-NLS-1$

    /* create control with selection buttons, style combo-boxes, edit button and delete button */
    m_resultLayerCommandData[ 0 ].setDocumentResult( m_documentResult );
    m_nodeStyleComp = new ResultStyleComposite( buttonComp, m_scenarioFolder, NodeResultHelper.NODE_TYPE, m_minValue, m_maxValue, m_resultLayerCommandData[0] );

    return buttonComp;
  }

  public void updateThemeCommandData( )
  {
    /* get infos about calc unit */
    final IResultMeta calcUnitMeta = m_documentResult.getParent().getParent();
    final IResultMeta timeStepMeta = m_documentResult.getParent();

    final IFolder resultsFolder = KalypsoModel1D2DHelper.getResultsFolder( m_scenarioFolder );
    final String resFolder = resultsFolder.getFullPath().toPortableString();

    final String featurePath = "nodeResultMember"; //$NON-NLS-1$
    final String source = "../" + m_documentResult.getFullPath().toPortableString(); //$NON-NLS-1$
//    final String style = "Vector Style"; //$NON-NLS-1$
    final String style = "Node Results Style"; //$NON-NLS-1$
    String styleLocation = null;
    final String type = NodeResultHelper.NODE_TYPE; 
    final String resultType = "gml"; //$NON-NLS-1$

    // check, if there is a style already chosen, if not create one from default template
    if( m_nodeStyleComp == null )
    {
      styleLocation = getStyle( resFolder, type );
    }
    final String lStyleFileName = ( new File( styleLocation == null? ResultMeta1d2dHelper.getDefaultStyleFileName( type, m_documentResult.getDocumentType().name() ): styleLocation ) ).getName();
    final String themeName = ResultMeta1d2dHelper.getNodeResultLayerName( m_documentResult, timeStepMeta, calcUnitMeta, ResultMeta1d2dHelper.resolveResultTypeFromSldFileName( lStyleFileName, type ) );

    if( m_resultLayerCommandData[0] != null )
      m_resultLayerCommandData[0].setValues( themeName, resultType, featurePath, source, style, styleLocation, type );
    else
      m_resultLayerCommandData[0] = new ResultAddLayerCommandData( themeName, resultType, featurePath, source, style, styleLocation, m_scenarioFolder, type );
  
    m_resultLayerCommandData[0].setProperty( IKalypsoTheme.PROPERTY_DELETEABLE, Boolean.toString( true ) );
    m_resultLayerCommandData[0].setProperty( IKalypsoTheme.PROPERTY_THEME_INFO_ID, THEME_INFO_ID );

  }

  private String getStyle( final String resFolder, final String type )
  {

    final String defaultPath = KalypsoModel1D2DHelper.getStylesFolder( m_scenarioFolder ).getFullPath().toPortableString();
    final String relativePathTo = FileUtilities.getRelativePathTo( resFolder, defaultPath );

    /* default location depending on type */
    final IFolder stylesFolder = KalypsoModel1D2DHelper.getStylesFolder( m_scenarioFolder );
    final IFolder sldFolder = stylesFolder.getFolder( type );

    final String sldFileName = ResultMeta1d2dHelper.getDefaultStyleFileName( type, m_documentResult.getDocumentType().name() );
    final String styleLocation = ".." + relativePathTo + "/" + type + "/" + sldFileName; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    final IFile styleFile = sldFolder.getFile( sldFileName );

    if( !ResultSldHelper.allDefaultNodeStylesExist( sldFolder ) )
//    if( styleFile.exists() == false )
    {
      ResultSldHelper.processStyle( styleFile, sldFolder, type, m_minValue, m_maxValue );
    }

    return styleLocation;
  }

  /**
   * @see org.kalypso.ui.wizards.results.IResultThemeCreator#createThemeCommandData(org.kalypso.template.gismapview.Gismapview)
   */
  @Override
  public ResultAddLayerCommandData[] getThemeCommandData( )
  {
    if( m_resultLayerCommandData != null )
      return m_resultLayerCommandData;
    else
      return null;
  }
}
