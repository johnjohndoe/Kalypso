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

import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.resources.IFolder;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DHelper;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.StepResultMeta;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;

/**
 * @author Thomas Jung
 * 
 */
public class NodeResultThemeCreator extends AbstractThemeCreator
{

  private final IDocumentResultMeta m_documentResult;

  private final IFolder m_scenarioFolder;

  List<ResultAddLayerCommandData> m_resultLayerCommandData = new LinkedList<ResultAddLayerCommandData>();

  private ResultStyleComposite m_nodeStyleComp;

  public NodeResultThemeCreator( IDocumentResultMeta documentResult, final IFolder scenarioFolder )
  {
    m_documentResult = documentResult;
    m_scenarioFolder = scenarioFolder;
  }

  @Override
  public Composite createControl( Group parent )
  {
    final Composite buttonComp = new Composite( parent, SWT.NONE );
    buttonComp.setLayout( new GridLayout( 4, false ) );

    final Label nodeLabel = new Label( buttonComp, SWT.FLAT );
    nodeLabel.setText( "Darstellung der Vektoren" );

    /* create control with selection buttons, style combo-boxes, edit button and delete button */
    m_nodeStyleComp = new ResultStyleComposite( buttonComp, m_scenarioFolder, "node" );

    return buttonComp;
  }

  public void createThemeCommandData( )
  {
    /* init */
    m_resultLayerCommandData.clear();

    /* fill */

    /* get infos about time step */
    StepResultMeta stepResultMeta = (StepResultMeta) m_documentResult.getParent();

    /* get infos about calc unit */
    IResultMeta calcUnitMeta = m_documentResult.getParent().getParent();

    final IFolder resultsFolder = KalypsoModel1D2DHelper.getResultsFolder( m_scenarioFolder );
    String resFolder = resultsFolder.getFullPath().toPortableString();

    String featurePath = "nodeResultMember";
    String source = "../" + m_documentResult.getFullPath().toPortableString();
    String style = "Vector Style";
    String themeName = m_documentResult.getName() + ", " + calcUnitMeta.getName();
    String styleLocation = null;

    if( m_nodeStyleComp != null )
    {
      final String absStyleLocation = m_nodeStyleComp.getSelectedStyle().getFullPath().toPortableString();
      final String relativePathTo = FileUtilities.getRelativePathTo( resFolder, absStyleLocation );
      styleLocation = ".." + relativePathTo;
    }
    else
    {
      // take the default style from the scenario style folder instead.
      final String defaultPath = KalypsoModel1D2DHelper.getStylesFolder( m_scenarioFolder ).getFullPath().toPortableString();
      final String relativePathTo = FileUtilities.getRelativePathTo( resFolder, defaultPath );
      styleLocation = ".." + relativePathTo + "/node/vector.sld";
    }

    String styleLinkType = "sld";
    String styleType = "simple";
    String resultType = "gml";

    m_resultLayerCommandData.add( new ResultAddLayerCommandData( themeName, resultType, featurePath, source, style, styleLocation, styleLinkType, styleType ) );

  }

  /**
   * @see org.kalypso.ui.wizards.results.IResultThemeCreator#createThemeCommandData(org.kalypso.template.gismapview.Gismapview)
   */
  @Override
  public ResultAddLayerCommandData[] getThemeCommandData( )
  {
    if( m_resultLayerCommandData != null )
      return m_resultLayerCommandData.toArray( new ResultAddLayerCommandData[m_resultLayerCommandData.size()] );
    else
      return null;
  }
}
