/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.ui.wizards.results;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.kalypso.kalypso1d2d.extension.Kalypso1d2dModule;
import org.kalypso.module.IKalypsoModule;
import org.kalypso.module.ModuleExtensions;

/**
 * @author Gernot Belger
 */
class Result1d2dMetaFilter extends ViewerFilter
{
  private final IKalypsoModule m_module1d2d = ModuleExtensions.getKalypsoModule( Kalypso1d2dModule.ID );

  @Override
  public boolean select( final Viewer viewer, final Object parentElement, final Object element )
  {
    if( element instanceof IProject )
      return selectProject( (IProject)element );

    return true;
  }

  private boolean selectProject( final IProject element )
  {
    try
    {
      return m_module1d2d.acceptProject( element );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
      return false;
    }
  }
}