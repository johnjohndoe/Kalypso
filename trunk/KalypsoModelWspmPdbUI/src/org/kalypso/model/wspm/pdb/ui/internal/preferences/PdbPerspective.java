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
package org.kalypso.model.wspm.pdb.ui.internal.preferences;

import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;
import org.kalypso.model.wspm.pdb.ui.internal.wspm.PdbMapViewPart;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartViewPart;
import org.kalypso.model.wspm.ui.view.table.TableView;
import org.kalypso.ogc.gml.outline.ViewContentOutline;
import org.kalypso.ui.perspectives.ModelerPerspectiveFactory;

/**
 * @author Gernot Belger
 */
public class PdbPerspective implements IPerspectiveFactory
{
  private static final String PDB_GMV_VIEW = "org.kalypso.model.wspm.pdb.ui.gmvView";

  private static final String LEFT_FOLDER = "pdbLeftFolder"; //$NON-NLS-1$

  private static final String OUTLINE_FOLDER = "outlineFolder"; //$NON-NLS-1$

  public static String ID = "PdbPerspective"; //$NON-NLS-1$

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

  private void defineLayout( final IPageLayout layout )
  {
    layout.setFixed( false );

    final String editorArea = layout.getEditorArea();
    layout.setEditorAreaVisible( false );

    layout.addView( PdbView.ID, IPageLayout.LEFT, 0.28f, editorArea );

    final IFolderLayout outlineFolder = layout.createFolder( OUTLINE_FOLDER, IPageLayout.BOTTOM, 0.50f, PdbView.ID );
    outlineFolder.addView( PDB_GMV_VIEW );
    outlineFolder.addView( ViewContentOutline.ID );

    layout.addView( PdbMapViewPart.ID, IPageLayout.RIGHT, 0.72f, editorArea );

    final IFolderLayout leftFolder = layout.createFolder( LEFT_FOLDER, IPageLayout.BOTTOM, 0.66f, PdbMapViewPart.ID );
    leftFolder.addView( ProfilChartViewPart.ID );
    leftFolder.addView( TableView.ID );

    layout.getViewLayout( PdbView.ID ).setCloseable( false );
    layout.getViewLayout( PdbView.ID ).setMoveable( false );

    layout.getViewLayout( ViewContentOutline.ID ).setCloseable( false );
    layout.getViewLayout( ViewContentOutline.ID ).setMoveable( false );

    layout.getViewLayout( PDB_GMV_VIEW ).setCloseable( false );
    layout.getViewLayout( PDB_GMV_VIEW ).setMoveable( false );

    layout.getViewLayout( PdbMapViewPart.ID ).setCloseable( false );
    layout.getViewLayout( PdbMapViewPart.ID ).setMoveable( false );

    layout.getViewLayout( ProfilChartViewPart.ID ).setCloseable( false );
    layout.getViewLayout( ProfilChartViewPart.ID ).setMoveable( false );

    layout.getViewLayout( TableView.ID ).setCloseable( false );
    layout.getViewLayout( TableView.ID ).setMoveable( false );
  }
}