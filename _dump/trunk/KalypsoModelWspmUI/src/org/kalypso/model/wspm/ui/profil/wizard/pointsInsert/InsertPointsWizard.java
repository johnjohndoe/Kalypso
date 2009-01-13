/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.ui.profil.wizard.pointsInsert;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;

/**
 * @author Gernot
 */
public class InsertPointsWizard extends Wizard
{
  protected final IProfil m_profile;

  protected PointsSourceChooserPage m_pointsSourceChooserPage;

  protected PointsTargetChooserPage m_pointsTargetChooserPage;

  public InsertPointsWizard( final IProfil profile )
  {
    m_profile = profile;

    final IDialogSettings wizardSettings = PluginUtilities.getDialogSettings( KalypsoModelWspmUIPlugin.getDefault(), "InsertPointsWizardSettings" ); //$NON-NLS-1$
    setDialogSettings( wizardSettings );

    setWindowTitle( org.kalypso.model.wspm.ui.i18n.Messages.getString("org.kalypso.model.wspm.ui.profil.wizard.pointsInsert.InsertPointsWizard.0") ); //$NON-NLS-1$
    setNeedsProgressMonitor( true );
  }

  @Override
  public void addPages( )
  {
    super.addPages();

    m_pointsSourceChooserPage = new PointsSourceChooserPage();
    m_pointsTargetChooserPage = new PointsTargetChooserPage();

    addPage( m_pointsSourceChooserPage );
    addPage( m_pointsTargetChooserPage );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    final IRunnableWithProgress runnable = new IRunnableWithProgress()
    {
      public void run( final IProgressMonitor monitor ) throws InvocationTargetException
      {
        final IPointsSource choosenSource = m_pointsSourceChooserPage.getChoosenSource();
        final IPointsTarget selectedTarget = m_pointsTargetChooserPage.getSelectedTarget();

        selectedTarget.insertPoints( m_profile, choosenSource.getPoints() );
      }
    };

    try
    {
      getContainer().run( false, true, runnable );
      return true;
    }
    // TODO: use helper methods from kalypso to extrakt status an show error dialog
    catch( final InvocationTargetException e )
    {
      e.printStackTrace();
      // TODO: error handling
      return false;
    }
    catch( final InterruptedException e )
    {
      e.printStackTrace();
      return false;
    }
  }
}
