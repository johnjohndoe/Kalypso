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
package org.kalypso.model.wspm.tuhh.ui.light;

import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;
import org.kalypso.chart.ui.view.ChartView;
import org.kalypso.featureview.views.FeatureView;
import org.kalypso.model.wspm.ui.view.LayerViewPart;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartViewPart;
import org.kalypso.model.wspm.ui.view.legend.LegendViewPart;
import org.kalypso.model.wspm.ui.view.table.TableView;
import org.kalypso.ogc.gml.outline.ViewContentOutline;
import org.kalypso.ui.editor.mapeditor.views.MapWidgetView;
import org.kalypso.ui.editor.mapeditor.views.StyleEditorViewPart;
import org.kalypso.ui.perspectives.ModelerPerspectiveFactory;

/**
 * @author Gernot Belger
 */
public class WspmLightPerspective implements IPerspectiveFactory
{
  public static final String ID = "WspmLightPerspective"; //$NON-NLS-1$

  private static final String MAIN_RIGHT_FOLDER = "wspmLightMainRightFolder"; //$NON-NLS-1$

  private static final String MAIN_FOLDER = "wspmLightMainFolder"; //$NON-NLS-1$

  private static final String TOP_LEFT = "wspmLightTopLeftFolder"; //$NON-NLS-1$

  private static final String BOTTOM_RIGHT = "wspmLightBottomRightFolder"; //$NON-NLS-1$

  private static final String SCND_BOTTOM_RIGHT = "wspmLightScndBottomRightFolder"; //$NON-NLS-1$

  private static final String BOTTOM_MAIN_FOLDER = "wspmLightBottomMainFolder"; //$NON-NLS-1$

  @Override
  public void createInitialLayout( final IPageLayout layout )
  {
    defineActions( layout );
    defineLayout( layout );
  }

  private void defineActions( final IPageLayout layout )
  {
    layout.addPerspectiveShortcut( ModelerPerspectiveFactory.ID );
  }

  /**
   * Layout of perspective
   * 
   * <pre>
   * ==================================================
   * =  TOP   |      MAIN          |      MAIN        =
   * =  LEFT  |     FOLDER         |      RIGHT       =
   * =        |                    |      FOLDER      =
   * =        |                    |                  =
   * =        |---------------------------------------=
   * =        |  BOTTOM MAIN       | BOTTOM | BOTTOM  =
   * =        |    FOLDER          | RIGHT  | RIGHT 2 =
   * ==================================================
   * </pre>
   */

  private void defineLayout( final IPageLayout layout )
  {
    layout.setFixed( false );

    final String editorArea = layout.getEditorArea();
    layout.setEditorAreaVisible( false );

    /** top left folder */
    final IFolderLayout leftFolder = layout.createFolder( TOP_LEFT, IPageLayout.LEFT, 0.15f, editorArea );
    leftFolder.addView( WspmGmvViewPart.ID );
    leftFolder.addPlaceholder( StyleEditorViewPart.ID );

    /** main view */
    final IFolderLayout mainFolder = layout.createFolder( MAIN_FOLDER, IPageLayout.RIGHT, 1.0f, editorArea );
    mainFolder.addView( WspmMapViewPart.ID );
    mainFolder.addView( TableView.ID );
    mainFolder.addPlaceholder( FeatureView.ID );
    mainFolder.addPlaceholder( ChartView.ID );

    /** bottom main folder */
    final IFolderLayout bottomMainFolder = layout.createFolder( BOTTOM_MAIN_FOLDER, IPageLayout.BOTTOM, 0.7f, MAIN_FOLDER );
    bottomMainFolder.addView( ProfilChartViewPart.ID );

    /** bottom right main folders */
    final IFolderLayout bottomRight = layout.createFolder( BOTTOM_RIGHT, IPageLayout.RIGHT, 0.7f, BOTTOM_MAIN_FOLDER );
    bottomRight.addView( LegendViewPart.class.getName() );

    final IFolderLayout scndBottomRight = layout.createFolder( SCND_BOTTOM_RIGHT, IPageLayout.RIGHT, 0.5f, BOTTOM_RIGHT );
    scndBottomRight.addView( LayerViewPart.class.getName() );

    /** main right */
    final IFolderLayout mainRightFolder = layout.createFolder( MAIN_RIGHT_FOLDER, IPageLayout.RIGHT, 0.7f, MAIN_FOLDER );
    mainRightFolder.addView( ViewContentOutline.ID );
    mainRightFolder.addPlaceholder( MapWidgetView.ID );
    mainRightFolder.addPlaceholder( "org.eclipse.help.ui.HelpView" ); //$NON-NLS-1$
    // mainRightFolder.addPlaceholder( IPageLayout.ID_PROGRESS_VIEW );

    layout.getViewLayout( WspmGmvViewPart.ID ).setCloseable( false );
    layout.getViewLayout( WspmGmvViewPart.ID ).setMoveable( false );

    layout.getViewLayout( WspmMapViewPart.ID ).setCloseable( false );
    layout.getViewLayout( WspmMapViewPart.ID ).setMoveable( false );

    layout.getViewLayout( ViewContentOutline.ID ).setCloseable( false );
    // layout.getViewLayout( ViewContentOutline.ID ).setMoveable( false );

    layout.getViewLayout( ProfilChartViewPart.ID ).setCloseable( false );
    // layout.getViewLayout( ProfilChartViewPart.ID ).setMoveable( false );

    layout.getViewLayout( ProfilChartViewPart.ID ).setCloseable( false );
    // layout.getViewLayout( ProfilChartViewPart.ID ).setMoveable( false );

    layout.getViewLayout( TableView.ID ).setCloseable( false );
    // layout.getViewLayout( TableView.ID ).setMoveable( false );

    layout.getViewLayout( LegendViewPart.class.getName() ).setCloseable( false );
// layout.getViewLayout( LegendViewPart.class.getName() ).setMoveable( false );

    layout.getViewLayout( LayerViewPart.class.getName() ).setCloseable( false );
// layout.getViewLayout( LayerViewPart.class.getName() ).setMoveable( false );
  }
}