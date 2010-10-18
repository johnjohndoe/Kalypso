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

package org.kalypso.ui.rrm.wizards.conversion.ui;

import java.io.File;
import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.afgui.wizards.NewProjectWizard;
import org.kalypso.contribs.eclipse.jface.wizard.ProjectTemplatePage;
import org.kalypso.ui.rrm.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.extension.KalypsoModuleRRM;
import org.kalypso.ui.rrm.i18n.Messages;
import org.kalypso.ui.rrm.wizards.KalypsoNAProjectWizard;
import org.kalypso.ui.rrm.wizards.conversion.ConverterUtils;
import org.kalypso.ui.rrm.wizards.conversion.IProjectConverter;
import org.kalypso.ui.rrm.wizards.conversion.IProjectConverterFactory;
import org.kalypso.ui.rrm.wizards.conversion.ProjectConversionOperation;
import org.kalypso.ui.rrm.wizards.conversion.from103to230.RrmProjectConverterFactory103to230;
import org.kalypso.ui.rrm.wizards.conversion.from210to230.RrmProjectConverterFactory210to230;

/**
 * This wizard converts project of old KalypsoHydrology versions into the current Kalypso version by creating a new
 * project and copying the the old data to the right places.<br/>
 * 
 * @author Gernot Belger
 */
public class KalypsoNAConvertProjectWizard extends NewProjectWizard
{
  private ProjectConversionPage m_conversionPage;

  public KalypsoNAConvertProjectWizard( )
  {
    super( new ProjectTemplatePage( "Projektvorlage", "Wie w‰hlen Sie, welche Projektvorlage verwendet werden soll", KalypsoNAProjectWizard.CATEGORY_TEMPLATE ), true, KalypsoModuleRRM.ID );

    setHelpAvailable( false );
    setNeedsProgressMonitor( true );
    setWindowTitle( Messages.getString( "KalypsoNAProjectWizard.9" ) ); //$NON-NLS-1$
  }

  @Override
  public void addPages( )
  {
    super.addPages();

    m_conversionPage = new ProjectConversionPage( "conversionPage" );

    addPage( m_conversionPage ); //$NON-NLS-1$

    // TODO: conversion pages:
    // - choose conversion parameters?
    // - choose old version (if not known)
  }

  /**
   * @see org.kalypso.afgui.wizards.NewProjectWizard#postCreateProject(org.eclipse.core.resources.IProject,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public void postCreateProject( final IProject project, final IProgressMonitor monitor ) throws CoreException
  {
    final File inputDir = m_conversionPage.getProjectDir();
    final IStatus result = doConvertProject( inputDir, project, monitor );
    if( !result.isOK() )
      throw new CoreException( result );
  }

  private IStatus doConvertProject( final File sourceDir, final IProject targetProject, final IProgressMonitor monitor )
  {
    try
    {
      final File targetDir = targetProject.getLocation().toFile();

      // FIXME: let user choose converters on project page; or: determine correct converter via project-version-number

      final IProjectConverterFactory[] factories = new IProjectConverterFactory[] { new RrmProjectConverterFactory103to230(), new RrmProjectConverterFactory210to230() };
      final IProjectConverter[] converters = ConverterUtils.createConverters( factories, sourceDir, targetDir );

      final ProjectConversionOperation operation = new ProjectConversionOperation( targetProject, converters );
      return operation.execute( monitor );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
      return e.getStatus();
    }
    catch( final InvocationTargetException e )
    {
      e.printStackTrace();
      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Unexpected error during project conversion", e );
    }
    catch( final InterruptedException e )
    {
      return Status.CANCEL_STATUS;
    }
  }
}
