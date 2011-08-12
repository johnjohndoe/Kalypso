/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.pdb.ui.internal.admin;

import org.apache.commons.lang.ObjectUtils;
import org.eclipse.core.expressions.PropertyTester;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PDBRole;
import org.kalypso.model.wspm.pdb.ui.internal.content.PdbView;

/**
 * @author Gernot Belger
 */
public class PdbPropertyTester extends PropertyTester
{
  public final static String PROPERTY_HAS_ROLE = "hasRole"; //$NON-NLS-1$

  @Override
  public boolean test( final Object receiver, final String property, final Object[] args, final Object expectedValue )
  {
    if( !(receiver instanceof IWorkbenchWindow) )
      return false;

    final IWorkbenchWindow window = (IWorkbenchWindow) receiver;
    final IWorkbenchPage page = window.getActivePage();
    if( page == null )
      return false;

    final IViewPart view = page.findView( PdbView.ID );
    if( !(view instanceof PdbView) )
      return false;

    final PdbView pdbView = (PdbView) view;

    if( PROPERTY_HAS_ROLE.equals( property ) )
      return testRole( pdbView, ObjectUtils.toString( expectedValue ) );

    return false;
  }

  private boolean testRole( final PdbView view, final String expectedRole )
  {
    final IPdbConnection connection = view.getConnection();
    if( connection == null || !connection.isConnected() )
      return false;

    final PDBRole role = connection.getRole();
    final String roleName = role.name();
    return expectedRole.equalsIgnoreCase( roleName );
  }
}