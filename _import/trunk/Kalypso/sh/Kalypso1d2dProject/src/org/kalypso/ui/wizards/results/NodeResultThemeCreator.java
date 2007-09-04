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
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DHelper;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;

/**
 * @author Thomas Jung
 * 
 */
public class NodeResultThemeCreator extends AbstractThemeCreator
{

  private final IDocumentResultMeta m_documentResult;

  private final IFolder m_scenarioFolder;

  private final ResultAddLayerCommandData[] m_resultLayerCommandData = new ResultAddLayerCommandData[1];

  private ResultStyleComposite m_nodeStyleComp;

  private final BigDecimal m_minValue;

  private final BigDecimal m_maxValue;

  public NodeResultThemeCreator( IDocumentResultMeta documentResult, final IFolder scenarioFolder )
  {
    m_documentResult = documentResult;
    m_minValue = m_documentResult.getMinValue();
    m_maxValue = m_documentResult.getMaxValue();
    m_scenarioFolder = scenarioFolder;

    updateThemeCommandData();
  }

  @Override
  public Composite createControl( Group parent )
  {
    final Composite buttonComp = new Composite( parent, SWT.NONE );
    buttonComp.setLayout( new GridLayout( 4, false ) );

    final Label nodeLabel = new Label( buttonComp, SWT.FLAT );
    nodeLabel.setText( "Darstellung der Vektoren" );

    /* create control with selection buttons, style combo-boxes, edit button and delete button */
    m_nodeStyleComp = new ResultStyleComposite( buttonComp, m_scenarioFolder, "Node", m_minValue, m_maxValue, m_resultLayerCommandData[0] );

    return buttonComp;
  }

  public void updateThemeCommandData( )
  {
    /* get infos about calc unit */
    IResultMeta calcUnitMeta = m_documentResult.getParent().getParent();

    final IFolder resultsFolder = KalypsoModel1D2DHelper.getResultsFolder( m_scenarioFolder );
    String resFolder = resultsFolder.getFullPath().toPortableString();

    String featurePath = "nodeResultMember";
    String source = "../" + m_documentResult.getFullPath().toPortableString();
    String style = "Vector Style";
    String themeName = m_documentResult.getName() + ", " + calcUnitMeta.getName();
    String styleLocation = null;
    String type = "Node";
    String styleLinkType = "sld";
    String styleType = "simple";
    String resultType = "gml";

    // check, if there is a style already chosen, if not create one from default template
    if( m_nodeStyleComp == null )
    {
      styleLocation = getStyle( resFolder, type );
    }

    if( m_resultLayerCommandData[0] != null )
      m_resultLayerCommandData[0].setValues( themeName, resultType, featurePath, source, style, styleLocation, styleLinkType, styleType, type );
    else
      m_resultLayerCommandData[0] = new ResultAddLayerCommandData( themeName, resultType, featurePath, source, style, styleLocation, styleLinkType, styleType, m_scenarioFolder, type );

  }

  private String getStyle( String resFolder, String type )
  {

    final String defaultPath = KalypsoModel1D2DHelper.getStylesFolder( m_scenarioFolder ).getFullPath().toPortableString();
    final String relativePathTo = FileUtilities.getRelativePathTo( resFolder, defaultPath );

    /* default location depending on type */
    final IFolder stylesFolder = KalypsoModel1D2DHelper.getStylesFolder( m_scenarioFolder );
    IFolder sldFolder = stylesFolder.getFolder( type );

    final String sldFileName = "default" + type + m_documentResult.getDocumentType().name() + "Style.sld";
    final String styleLocation = ".." + relativePathTo + "/" + type + "/" + sldFileName;

    final IFile styleFile = sldFolder.getFile( sldFileName );

    if( styleFile.exists() == false )
    {
      ResultSldHelper.processStyle( styleFile, type, m_minValue, m_maxValue );
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
