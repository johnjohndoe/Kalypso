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
package org.kalypso.core.gml.provider;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;

/**
 * This wizard lets the user choose from various available gml sources.
 * 
 * @author Gernot Belger
 */
public class GmlSourceChooserWizard extends Wizard
{
  private final IGmlSourceProvider[] m_provider;

  private final GmlSourceChooserPage m_page;

  private final IGmlSourceRunnableWithProgress m_operation;

  /**
   * @param operation
   *            This operation will be executed when the finish button is pressed, using the choosen {@link IGmlSource}s.
   */
  public GmlSourceChooserWizard( final IGmlSourceProvider[] provider, final IGmlSourceRunnableWithProgress operation )
  {
    m_provider = provider;
    m_operation = operation;

    setForcePreviousAndNextButtons( false );
    setNeedsProgressMonitor( true );

    setDialogSettings( getDialogSettings() );

    m_page = new GmlSourceChooserPage( "sourceProviderPage", "Auswahl", null, m_provider );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {
    addPage( m_page );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    m_operation.setGmlSource( m_page.getChoosenSources() );

    final IStatus resultStatus = RunnableContextHelper.execute( getContainer(), true, true, m_operation );
    return m_operation.handleResult( getShell(), resultStatus );
  }
}
