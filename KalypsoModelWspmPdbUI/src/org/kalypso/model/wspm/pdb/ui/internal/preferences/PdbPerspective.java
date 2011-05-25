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

import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;
import org.kalypso.model.wspm.pdb.ui.internal.admin.info.PdbInfoView;
import org.kalypso.ui.perspectives.ModelerPerspectiveFactory;

/**
 * @author Gernot Belger
 */
public class PdbPerspective implements IPerspectiveFactory
{
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
    // FIXME:
    layout.setFixed( false );

    final String editorArea = layout.getEditorArea();
    layout.setEditorAreaVisible( false );

    /* Add the manager view. */
    layout.addView( PdbView.ID, IPageLayout.LEFT, 0.28f, editorArea );
    layout.addView( PdbInfoView.ID, IPageLayout.RIGHT, 0.72f, editorArea );

    layout.getViewLayout( PdbView.ID ).setCloseable( false );
    layout.getViewLayout( PdbView.ID ).setMoveable( false );

    layout.getViewLayout( PdbInfoView.ID ).setCloseable( false );
    layout.getViewLayout( PdbInfoView.ID ).setMoveable( false );

    /* final IFolderLayout folder = */
    // layout.createFolder( "rest", IPageLayout.RIGHT, 0.72f, editorArea );
    // folder.addView( MapView.ID );
    // folder.addView( MapView.ID );
  }
}