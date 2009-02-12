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
package org.kalypso.contribs.eclipse.jface.wizard;

import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.wizard.IWizardContainer2;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;

/**
 * This implementation of {@link org.eclipse.jface.wizard.IWizardContainer}is just a
 * {@link org.eclipse.swt.widgets.Composite}.
 * 
 * 
 * @author belger
 */
public class WizardComposite extends Composite implements IWizardContainer2
{
  //  private final IWizard m_wizard;

  public WizardComposite( /** final IWizard wizard, */
  final Composite parent, final int style )
  {
    super( parent, style );

    //    m_wizard = wizard;

    setLayout( new GridLayout() );
  }

  /**
   * @see org.eclipse.jface.wizard.IWizardContainer2#updateSize()
   */
  public void updateSize()
  {
  //  
  }

  /**
   * @see org.eclipse.jface.wizard.IWizardContainer#getCurrentPage()
   */
  public IWizardPage getCurrentPage()
  {
    return null;
  }

  /**
   * @see org.eclipse.jface.wizard.IWizardContainer#getShell()
   */
  public Shell getShell()
  {
    return null;
  }

  /**
   * @see org.eclipse.jface.wizard.IWizardContainer#showPage(org.eclipse.jface.wizard.IWizardPage)
   */
  public void showPage( IWizardPage page )
  {
  //  
  }

  /**
   * @see org.eclipse.jface.wizard.IWizardContainer#updateButtons()
   */
  public void updateButtons()
  {
  //  
  }

  /**
   * @see org.eclipse.jface.wizard.IWizardContainer#updateMessage()
   */
  public void updateMessage()
  {
  //  
  }

  /**
   * @see org.eclipse.jface.wizard.IWizardContainer#updateTitleBar()
   */
  public void updateTitleBar()
  {
  //  
  }

  /**
   * @see org.eclipse.jface.wizard.IWizardContainer#updateWindowTitle()
   */
  public void updateWindowTitle()
  {
  //  
  }

  /**
   * @see org.eclipse.jface.operation.IRunnableContext#run(boolean, boolean,
   *      org.eclipse.jface.operation.IRunnableWithProgress)
   */
  public void run( boolean fork, boolean cancelable, IRunnableWithProgress runnable )
  //throws InvocationTargetException, InterruptedException
  {
  //  
  }

}
