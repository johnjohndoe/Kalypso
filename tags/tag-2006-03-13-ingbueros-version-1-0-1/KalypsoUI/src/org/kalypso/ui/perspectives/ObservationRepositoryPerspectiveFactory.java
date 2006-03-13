/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.perspectives;

import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;
import org.kalypso.ui.IKalypsoUIConstants;

/**
 * The PerspectiveFactory for browsing the ObservationRepository.
 * 
 * @author schlienger
 */
public class ObservationRepositoryPerspectiveFactory implements IPerspectiveFactory
{
  /**
   * @see IPerspectiveFactory#createInitialLayout(IPageLayout)
   */
  public void createInitialLayout( final IPageLayout layout )
  {
    final IFolderLayout topLeft = layout
        .createFolder( "topLeft", IPageLayout.LEFT, (float)0.26, layout.getEditorArea() );
    topLeft.addView( IKalypsoUIConstants.ID_REPOSITORY_VIEW );
    topLeft.addPlaceholder( IPageLayout.ID_RES_NAV );

    final IFolderLayout botLeft = layout.createFolder( "bottom", IPageLayout.BOTTOM, (float)0.70, "topLeft" );
    botLeft.addView( IPageLayout.ID_PROP_SHEET );

    final IFolderLayout leftBottom = layout.createFolder( "leftBottom", IPageLayout.BOTTOM, (float)0.0, layout
        .getEditorArea() );
    leftBottom.addView( IKalypsoUIConstants.ID_OBSDIAGRAM_VIEW );

    final IFolderLayout rightBottom = layout.createFolder( "rightBottom", IPageLayout.RIGHT, (float)0.50, "leftBottom" );
    rightBottom.addView( IKalypsoUIConstants.ID_OBSTABLE_VIEW );

    setContentsOfShowViewMenu( layout );
    layout.setEditorAreaVisible( false );
  }

  /**
   * Sets the intial contents of the "Show View" menu
   * 
   * @param layout
   */
  protected void setContentsOfShowViewMenu( IPageLayout layout )
  {
    layout.addShowViewShortcut( IPageLayout.ID_OUTLINE );
    layout.addShowViewShortcut( IKalypsoUIConstants.ID_REPOSITORY_VIEW );
    layout.addShowViewShortcut( IKalypsoUIConstants.ID_OBSDIAGRAM_VIEW );
    layout.addShowViewShortcut( IKalypsoUIConstants.ID_OBSTABLE_VIEW );
  }
}