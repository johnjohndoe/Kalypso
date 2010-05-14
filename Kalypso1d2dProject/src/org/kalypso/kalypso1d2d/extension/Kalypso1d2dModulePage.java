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
package org.kalypso.kalypso1d2d.extension;

import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.jface.wizard.IWizard;
import org.kalypso.afgui.wizards.INewProjectWizard;
import org.kalypso.kalypso1d2d.pjt.Kalypso1D2DDemoProjectWizard;
import org.kalypso.kalypso1d2d.pjt.Kalypso1D2DNewProjectWizard;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.project.database.client.extension.IKalypsoModule;
import org.kalypso.project.database.client.extension.pages.module.AbstractKalypsoModulePage;

/**
 * @author kuch
 */
public class Kalypso1d2dModulePage extends AbstractKalypsoModulePage
{
  public Kalypso1d2dModulePage( final IKalypsoModule module )
  {
    super( module );
  }

  public String getHeader( )
  {
    return "Kalypso1D2D"; //$NON-NLS-1$
  }

  public URL getInfoURL( ) throws MalformedURLException
  {
    return getInfoURL( getClass(), Kalypso1d2dProjectPlugin.getDefault() );
  }

  public INewProjectWizard getDemoProjectWizard( )
  {
    return new Kalypso1D2DDemoProjectWizard();
  }

  /**
   * @see org.kalypso.afgui.extension.IKalypsoModuleEnteringPageHandler#hasDemoProjectWizard()
   */
  public boolean hasDemoProjectWizard( )
  {
    return true;
  }

  public INewProjectWizard getProjectWizard( )
  {
    return new Kalypso1D2DNewProjectWizard();
  }

  public IWizard getImportWizard( )
  {
    return null;
  }

  /**
   * @see org.kalypso.afgui.extension.IKalypsoModuleEnteringPageHandler#hasImportWizard()
   */
  public boolean hasImportWizard( )
  {
    return false;
  }

  public String getImportWizardLabel( )
  {
    return null;
  }

  public Integer getPriority( )
  {
    return 3;
  }

}
