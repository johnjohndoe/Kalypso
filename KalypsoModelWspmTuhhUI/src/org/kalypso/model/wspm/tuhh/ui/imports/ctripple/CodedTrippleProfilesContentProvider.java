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
package org.kalypso.model.wspm.tuhh.ui.imports.ctripple;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.kalypso.model.wspm.tuhh.core.ctripple.CodedTripple;
import org.kalypso.model.wspm.tuhh.core.ctripple.CodedTrippleProfile;

/**
 * @author Holger Albert
 */
public class CodedTrippleProfilesContentProvider implements ITreeContentProvider
{
  public CodedTrippleProfilesContentProvider( )
  {
  }

  @Override
  public void dispose( )
  {
  }

  @Override
  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
  {
  }

  @Override
  public Object[] getElements( final Object inputElement )
  {
    /* CASE: CodedTrippleProfile[] was set as input. */
    if( inputElement instanceof CodedTrippleProfile[] )
      return (Object[])inputElement;

    /* CASE: CodedTripple was set as input. */
    if( inputElement instanceof CodedTripple )
    {
      final CodedTripple codedTripple = (CodedTripple)inputElement;
      return codedTripple.getProfiles();
    }

    /* CASE: null or something unknown was set as input. */
    return new Object[] {};
  }

  @Override
  public Object[] getChildren( final Object parentElement )
  {
    return null;
  }

  @Override
  public Object getParent( final Object element )
  {
    return null;
  }

  @Override
  public boolean hasChildren( final Object element )
  {
    return false;
  }
}