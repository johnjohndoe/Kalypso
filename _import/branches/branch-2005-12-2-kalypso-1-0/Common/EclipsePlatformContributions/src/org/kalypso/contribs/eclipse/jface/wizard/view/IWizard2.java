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
package org.kalypso.contribs.eclipse.jface.wizard.view;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.IWizardPage;

/**
 * Adds some additional methods to the {@link IWizard}
 * 
 * @author belger
 */
public interface IWizard2 extends IWizard
{
  /**
   * Finishes a single page
   * 
   * @return false, if something went wrong. Don't change the page now.
   */
  public boolean finishPage( final IWizardPage page );
  
  public boolean hasCancelButton();
  
  /** The initial browser size in percent of the whole area */
  public int getInitialBrowserSize();
  
  /** The context-id which to show if help is invoked */
  public String getHelpId();

  /**
   * Saves the contents/state of all pages.
   * @throws CoreException
   */
  public IStatus saveAllPages( final IProgressMonitor monitor ) throws CoreException;

  /**
   * @return Return <code>true</code>, if a 'save' button should be visible for this wizard.
   */
  public boolean hasSaveButton();

  /**
   * @return If the user should be aksed before any save. 
   */
  public boolean doAskForSave();
}
