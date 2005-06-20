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
package org.kalypso.ui.calcwizard.createpages;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

/**
 * @author belger
 */
public interface IAddCalcCaseChoice
{
  public void createControl( final Composite parent );

  public Control getControl();

  /**
   * Erzeugt oder wählt eine Rechenvariante und gibt derem Basisverzeichnis zurück
   * 
   * @param monitor
   * @return Gewählte Rechenvariante
   * @throws CoreException
   */
  public IFolder perform( final IProgressMonitor monitor ) throws CoreException;

  /**
   * @see java.lang.Object#toString()
   */
  public String toString();

  /**
   * Die View refreshen, mittlerweile können Sich die benutzten und vorhandenen Rechenfälle geändert haben
   * 
   * @param monitor
   * @throws CoreException
   */
  public void refresh( final IProgressMonitor monitor ) throws CoreException;

  /**
   * Ob bei dieser Wahl der Rechenvariante nach Eingabe der Steuerparameter aktualisiert werden sollte
   * 
   * @return true wenn Aktualisierung erfolgen soll
   */
  public boolean shouldUpdate();

  public void validateChoice();
}