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
package org.kalypso.model.wspm.tuhh.ui.imports;

import java.io.File;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.model.wspm.tuhh.core.profile.importer.wprof.IProfileCreatorStrategy;
import org.kalypso.model.wspm.tuhh.core.profile.importer.wprof.ProfileCreatorStrategy;
import org.kalypso.model.wspm.tuhh.core.profile.importer.wprof.SoilOnlyProfileCreatorStrategy;
import org.kalypso.model.wspm.tuhh.core.profile.importer.wprof.TuhhProfileWProfContentHandler;
import org.kalypso.model.wspm.tuhh.core.profile.importer.wprof.WProfImportOperation;
import org.kalypso.model.wspm.tuhh.core.wprof.BCEShapeWPRofContentProviderFactory;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.KalypsoDeegreePlugin;

/**
 * @author Gernot Belger
 */
public class WProfImportWizard extends Wizard
{
  private final BCEShapeWPRofContentProviderFactory m_pointFactory = new BCEShapeWPRofContentProviderFactory();

  private final TuhhWspmProject m_targetProject;

  private final WProfImportFilePage m_wprofFilePage;

  private final CommandableWorkspace m_workspace;

  private final WProfOptionsPage m_wprofMarkerPage;

  private final WProfPropertyPage m_wprofPropertyPage;

  public WProfImportWizard( final CommandableWorkspace workspace, final TuhhWspmProject targetProject )
  {
    m_workspace = workspace;
    m_targetProject = targetProject;

    m_wprofFilePage = new WProfImportFilePage( "wprofFilePage", Messages.getString("WProfImportWizard_0"), null ); //$NON-NLS-1$ //$NON-NLS-2$
    m_wprofFilePage.setDescription( Messages.getString("WProfImportWizard_1") ); //$NON-NLS-1$

    m_wprofMarkerPage = new WProfOptionsPage( "wprofMarkerPage", Messages.getString("WProfImportWizard_2"), null ); //$NON-NLS-1$ //$NON-NLS-2$
    m_wprofMarkerPage.setDescription( Messages.getString("WProfImportWizard_3") ); //$NON-NLS-1$

    final WProfProfileStrategyOptions profileStrategyOptions = m_wprofMarkerPage.getProfileStrategyOptions();
    profileStrategyOptions.addStrategy( new ProfileCreatorStrategy() );
    profileStrategyOptions.addStrategy( new SoilOnlyProfileCreatorStrategy( false ) );
    profileStrategyOptions.addStrategy( new SoilOnlyProfileCreatorStrategy( true ) );

    final Properties defaultSpecification = m_pointFactory.getDefaultSpecification();
    m_wprofPropertyPage = new WProfPropertyPage( "wprofPropertyPage", defaultSpecification ); //$NON-NLS-1$

    setWindowTitle( Messages.getString("WProfImportWizard_4") ); //$NON-NLS-1$
    setNeedsProgressMonitor( true );

    addPage( m_wprofFilePage );
    addPage( m_wprofMarkerPage );
    addPage( m_wprofPropertyPage );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    final File shapeFile = m_wprofFilePage.getWProfFile();
    final String wprofPath = shapeFile.getAbsolutePath();

    final String targetSrs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    final String photoContext = m_wprofFilePage.getPhotoContext();
    final String pdfContext = m_wprofFilePage.getPdfContext();

    final TuhhProfileWProfContentHandler handler = new TuhhProfileWProfContentHandler( m_workspace, m_targetProject, targetSrs, wprofPath );

    final WProfProfileStrategyOptions profileStrategyOptions = m_wprofMarkerPage.getProfileStrategyOptions();
    final IProfileCreatorStrategy strategy = profileStrategyOptions.getCurrentStrategy();
    handler.setStrategy( strategy );

    final WProfMarkerOptions markerOptions = m_wprofMarkerPage.getMarkerOptions();
    final Map<String, int[]> markerMappings = markerOptions.getMarkerMappings();
    for( final Entry<String, int[]> mappingEntry : markerMappings.entrySet() )
    {
      final String markerID = mappingEntry.getKey();
      final int[] types = mappingEntry.getValue();
      for( final int type : types )
        handler.addMarkerMapping( markerID, type );
    }

    final Properties specification = m_wprofPropertyPage.getSpecification();
    m_pointFactory.setPdfContext( pdfContext );
    m_pointFactory.setPhotoContext( photoContext );
    m_pointFactory.setSpecification( specification );

    final WProfImportOperation op = new WProfImportOperation( shapeFile, handler, m_pointFactory );
    op.setShapeCharset( m_wprofFilePage.getShapeCharset() );
    op.setShapeDefaultSrs( m_wprofFilePage.getShapeDefaultSrs() );

    final IWizardContainer container = getContainer();
    final IStatus result = RunnableContextHelper.execute( container, true, true, op );
    ErrorDialog.openError( getShell(), getWindowTitle(), Messages.getString("WProfImportWizard_5"), result ); //$NON-NLS-1$

    return !result.matches( IStatus.ERROR );
  }
}
