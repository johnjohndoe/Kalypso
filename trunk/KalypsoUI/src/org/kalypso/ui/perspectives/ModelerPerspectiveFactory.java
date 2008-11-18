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
import org.eclipse.ui.internal.PageLayout;

/**
 * The perspective for the Kalypso Modeler.
 * 
 * @author schlienger
 */
public class ModelerPerspectiveFactory implements IPerspectiveFactory
{
  public static final String ID = "org.kalypso.ui.perspectives.ModelerPerspectiveFactory"; //$NON-NLS-1$
  
  /**
   * @see org.eclipse.ui.IPerspectiveFactory#createInitialLayout(org.eclipse.ui.IPageLayout)
   */
  @SuppressWarnings("restriction")
  public void createInitialLayout( final IPageLayout layout )
  {
    // Editors are placed for free.
    final String editorArea = layout.getEditorArea();

    // Top left.
    final IFolderLayout topLeft = layout.createFolder( "topLeft", IPageLayout.LEFT, (float) 0.26, editorArea );//$NON-NLS-1$
    topLeft.addView( IPageLayout.ID_RES_NAV );

    // Bottom left.
    final IFolderLayout bottomLeft = layout.createFolder( "bottomLeft", IPageLayout.BOTTOM, (float) 0.50,//$NON-NLS-1$
        "topLeft" );//$NON-NLS-1$
    bottomLeft.addView( IPageLayout.ID_OUTLINE );

    setContentsOfShowViewMenu( layout );

    // a bit dirty, but this perspective should be minimalistic
    if( layout instanceof PageLayout )
      ((PageLayout) layout).getActionSets().clear();
    
    layout.addActionSet( "org.kalypso.simulation.ui.actionSet" );

    layout.addNewWizardShortcut( "org.eclipse.ui.wizards.new.folder" );//$NON-NLS-1$
    layout.addNewWizardShortcut( "org.eclipse.ui.wizards.new.file" );//$NON-NLS-1$
  }

  /**
   * Sets the initial contents of the "Show View" menu
   * 
   * @param layout
   */
  protected void setContentsOfShowViewMenu( final IPageLayout layout )
  {
    layout.addShowViewShortcut( IPageLayout.ID_RES_NAV );
    layout.addShowViewShortcut( IPageLayout.ID_OUTLINE );
  }
}