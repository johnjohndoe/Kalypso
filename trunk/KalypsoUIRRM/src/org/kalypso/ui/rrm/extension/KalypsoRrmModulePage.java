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
package org.kalypso.ui.rrm.extension;

import java.net.URL;

import org.eclipse.jface.wizard.IWizard;
import org.kalypso.afgui.wizards.INewProjectWizard;
import org.kalypso.afgui.wizards.INewProjectWizardProvider;
import org.kalypso.project.database.client.extension.IKalypsoModule;
import org.kalypso.project.database.client.extension.pages.module.AbstractKalypsoModulePage;
import org.kalypso.ui.rrm.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.wizards.KalypsoNAProjectWizard;
import org.kalypso.ui.rrm.wizards.conversion.KalypsoNAConvertProjectWizard;

/**
 * @author kuch
 */
public class KalypsoRrmModulePage extends AbstractKalypsoModulePage
{
  public KalypsoRrmModulePage( final IKalypsoModule module )
  {
    super( module );
  }

  @Override
  public String getHeader( )
  {
    return "KalypsoHydrology"; //$NON-NLS-1$
  }

  @Override
  public URL getInfoURL( )
  {
    return getInfoURL( getClass(), KalypsoUIRRMPlugin.getDefault() );
  }

  @Override
  public INewProjectWizardProvider getDemoProjectWizard( )
  {
    return null;
  }

  /**
   * @see org.kalypso.afgui.extension.IKalypsoModuleEnteringPageHandler#hasDemoProjectWizard()
   */
  @Override
  public boolean hasDemoProjectWizard( )
  {
    return false;
  }

  @Override
  public INewProjectWizardProvider getProjectWizard( )
  {
    return new INewProjectWizardProvider()
    {
      @Override
      public INewProjectWizard createWizard( )
      {
        return new KalypsoNAProjectWizard();
      }
    };
  }

  /**
   * @see org.kalypso.afgui.extension.IKalypsoModuleEnteringPageHandler#hasImportWizard()
   */
  @Override
  public boolean hasImportWizard( )
  {
    return true;
  }

  @Override
  public IWizard getImportWizard( )
  {
    // FIXME: we need a more flexible means to add new icons to the bottom of the project list
    return new KalypsoNAConvertProjectWizard();
  }

  @Override
  public String getImportWizardLabel( )
  {
    return "Altprojekte importieren";
  }

  @Override
  public Integer getPriority( )
  {
    return 1;
  }
}
