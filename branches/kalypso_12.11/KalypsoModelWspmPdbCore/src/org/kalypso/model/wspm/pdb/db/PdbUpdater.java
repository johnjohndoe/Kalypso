/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.pdb.db;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.hibernate.jdbc.Work;
import org.kalypso.commons.patternreplace.ConstantReplacer;
import org.kalypso.commons.patternreplace.PatternInputReplacer;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PDBRole;
import org.kalypso.model.wspm.pdb.db.version.UpdateScript;
import org.kalypso.model.wspm.pdb.db.version.UpdateScriptExtenions;
import org.kalypso.model.wspm.pdb.db.version.UpdateScriptPageData;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.internal.update.SqlWork;
import org.kalypso.model.wspm.pdb.internal.update.UpdateScriptWizard;
import org.kalypso.model.wspm.pdb.internal.update.WorkRunnable;
import org.osgi.framework.Version;

import com.vividsolutions.jts.geom.Envelope;

/**
 * @author Gernot Belger
 */
public class PdbUpdater
{
  enum DbState
  {
    uptodate,
    empty,
    dbOutdated,
    kalypsoOutdated
  }

  private final static String WINDOW_TITLE = Messages.getString( "PdbUpdater_0" ); //$NON-NLS-1$

  private static final String STR_CONNECTION_IMPOSSIBLE = Messages.getString( "PdbUpdater_1" ); //$NON-NLS-1$

  private static final String STR_CREATE = STR_CONNECTION_IMPOSSIBLE + Messages.getString( "PdbUpdater_2" ); //$NON-NLS-1$

  private static final String STR_UPDATE = STR_CONNECTION_IMPOSSIBLE + Messages.getString( "PdbUpdater_3" ); //$NON-NLS-1$

  private final IPdbConnection m_connection;

  private final Shell m_shell;

  public PdbUpdater( final IPdbConnection connection, final Shell shell )
  {
    m_connection = connection;
    m_shell = shell;
  }

  public IStatus execute( )
  {
    /* Check version */
    final PdbInfo info = m_connection.getInfo();
    final boolean isSuperuser = checkSuperuser();
    final Version version = info.getVersion();
    final DbState state = checkDbState( version );

    switch( state )
    {
      case uptodate:
        return Status.OK_STATUS;

      case empty:
        if( isSuperuser )
          return handleCreate();
        else
          return handleShouldCreate();

      case kalypsoOutdated:
        final String msg = String.format( STR_CONNECTION_IMPOSSIBLE + Messages.getString( "PdbUpdater_4" ), version ); //$NON-NLS-1$
        return new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, msg );

      case dbOutdated:
        if( isSuperuser )
          return handleUpdate( version );
        else
          return handleShouldUpdate( version );
    }

    throw new IllegalStateException();
  }

  private DbState checkDbState( final Version version )
  {
    // TODO: check, if database is empty
    // TODO: check, if it is a spatial database
    if( version == null )
      return DbState.empty;

    if( PdbInfo.CURRENT_VERSION.equals( version ) )
      return DbState.uptodate;

    if( version.compareTo( PdbInfo.CURRENT_VERSION ) > 0 )
      return DbState.kalypsoOutdated;

    if( version.compareTo( PdbInfo.CURRENT_VERSION ) < 0 )
      return DbState.dbOutdated;

    throw new IllegalStateException();
  }

  private boolean checkSuperuser( )
  {
    return m_connection.getRole() == PDBRole.superuser;
  }

  private IStatus handleCreate( )
  {
    /* ask user what to do */
    final String msg = String.format( Messages.getString( "PdbUpdater_5" ), STR_CREATE ); //$NON-NLS-1$
    final MessageDialog dialog = new MessageDialog( m_shell, WINDOW_TITLE, null, msg, MessageDialog.CONFIRM, new String[] { Messages.getString( "PdbUpdater_6" ), IDialogConstants.CANCEL_LABEL }, 1 ); //$NON-NLS-1$
    if( dialog.open() != Window.OK )
      return Status.CANCEL_STATUS;

    /* Do update */
    final String dbType = m_connection.getSettings().getType();
    final UpdateScript createScript = UpdateScriptExtenions.getScript( new Version( 0, 0, 0 ), dbType );
    if( createScript == null )
      throw new IllegalStateException( "Missing create script" ); //$NON-NLS-1$
    final UpdateScript[] scripts = new UpdateScript[] { createScript };
    return executeScripts( scripts, Messages.getString( "PdbUpdater_7" ) ); //$NON-NLS-1$
  }

  private IStatus handleShouldCreate( )
  {
    final String msg = String.format( Messages.getString( "PdbUpdater_8" ), STR_CREATE, IPdbConnection.SUPERUSER ); //$NON-NLS-1$
    MessageDialog.openWarning( m_shell, WINDOW_TITLE, msg );
    return Status.CANCEL_STATUS;
  }

  private IStatus handleUpdate( final Version version )
  {
    // 0) ggf. check preconditions

    /* ask user what to do */
    final String baseMsg = String.format( STR_UPDATE, version, PdbInfo.CURRENT_VERSION );
    final String msg = baseMsg + Messages.getString( "PdbUpdater_9" ); //$NON-NLS-1$
    final MessageDialog dialog = new MessageDialog( m_shell, WINDOW_TITLE, null, msg, MessageDialog.CONFIRM, new String[] { Messages.getString( "PdbUpdater_10" ), IDialogConstants.CANCEL_LABEL }, 1 ); //$NON-NLS-1$
    if( dialog.open() != Window.OK )
      return Status.CANCEL_STATUS;

    /* Do update */
    final String dbType = m_connection.getSettings().getType();
    final UpdateScript[] scripts = UpdateScriptExtenions.getUpdateScripts( version, dbType );
    if( scripts == null )
      throw new IllegalStateException( "Missing create script" ); //$NON-NLS-1$
    return executeScripts( scripts, Messages.getString( "PdbUpdater_11" ) ); //$NON-NLS-1$
  }

  private IStatus handleShouldUpdate( final Version version )
  {
    final String baseMsg = String.format( STR_UPDATE, version, PdbInfo.CURRENT_VERSION );
    final String msg = String.format( Messages.getString( "PdbUpdater_12" ), baseMsg, IPdbConnection.SUPERUSER ); //$NON-NLS-1$
    MessageDialog.openWarning( m_shell, WINDOW_TITLE, msg );
    return Status.CANCEL_STATUS;
  }

  private IStatus executeScripts( final UpdateScript[] scripts, final String windowTitle )
  {
    try
    {
      final Properties replaceVariables = determineVariables( scripts );
      final String[] sqls = loadSql( scripts, replaceVariables );
      if( sqls == null )
        return Status.OK_STATUS;

      final Work operation = new SqlWork( sqls );
      final WorkRunnable runnable = new WorkRunnable( m_connection, operation );

      final IStatus status = ProgressUtilities.busyCursorWhile( runnable );
      if( !status.isOK() )
        new StatusDialog( m_shell, status, windowTitle ).open();

      m_connection.updateInfo();

      return status;
    }
    catch( final CoreException e )
    {
      if( !e.getStatus().matches( IStatus.CANCEL ) )
        e.printStackTrace();
      return e.getStatus();
    }
  }

  private String[] loadSql( final UpdateScript[] scripts, final Properties variables )
  {
    try
    {
      final PatternInputReplacer<Object> inputReplacer = configurePatternReplacer( variables );

      final Collection<String> sql = new ArrayList<>();
      for( final UpdateScript script : scripts )
      {
        final String[] sqlStatements = script.loadSQL();
        for( final String statement : sqlStatements )
        {
          final String resolvedStatement = inputReplacer.replaceTokens( statement, null );
          sql.add( resolvedStatement );
        }
      }
      return sql.toArray( new String[sql.size()] );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      throw new IllegalStateException( Messages.getString( "PdbUpdater_13" ), e ); //$NON-NLS-1$
    }
  }

  private PatternInputReplacer<Object> configurePatternReplacer( final Properties variables )
  {
    final PatternInputReplacer<Object> inputReplacer = new PatternInputReplacer<>( "${", "}" ); //$NON-NLS-1$ //$NON-NLS-2$

    final Set<String> names = variables.stringPropertyNames();
    for( final String name : names )
    {
      final String value = variables.getProperty( name );

      // REMARK: '\' causes problem, at least with postgres, and is only every used in pathes. So we replace it with '/' here.
      final String cleanValue = value.replace( '\\', '/' );

      inputReplacer.addReplacer( new ConstantReplacer( name, cleanValue ) );
    }

    return inputReplacer;
  }

  private Properties determineVariables( final UpdateScript[] scripts ) throws CoreException
  {
    final Properties properties = new Properties();

    /* some defaults */
    properties.setProperty( PdbInfo.PROPERTY_DOCUMENT_SERVER, "http://example.com/document/path/" ); //$NON-NLS-1$
    properties.setProperty( PdbInfo.PROPERTY_DEM_SERVER, "P:/data/terrain/" ); //$NON-NLS-1$
    properties.setProperty( PdbInfo.PROPERTY_SRID, "31468" ); //$NON-NLS-1$

    /* Add depending variables */
    final String srid = properties.getProperty( PdbInfo.PROPERTY_SRID );
    final Envelope domainOfValidity = m_connection.getCrsEnvelope( Integer.valueOf( srid ) );

    properties.setProperty( PdbInfo.PROPERTY_SRS_MIN_Z, String.format( Locale.US, "%f", -1000.0 ) ); //$NON-NLS-1$
    properties.setProperty( PdbInfo.PROPERTY_SRS_MAX_Z, String.format( Locale.US, "%f", 10000.0 ) ); //$NON-NLS-1$

    if( domainOfValidity != null )
    {
      properties.setProperty( PdbInfo.PROPERTY_SRS_MIN_X, String.format( Locale.US, "%f", domainOfValidity.getMinX() ) ); //$NON-NLS-1$
      properties.setProperty( PdbInfo.PROPERTY_SRS_MAX_X, String.format( Locale.US, "%f", domainOfValidity.getMaxX() ) ); //$NON-NLS-1$

      properties.setProperty( PdbInfo.PROPERTY_SRS_MIN_Y, String.format( Locale.US, "%f", domainOfValidity.getMinY() ) ); //$NON-NLS-1$
      properties.setProperty( PdbInfo.PROPERTY_SRS_MAX_Y, String.format( Locale.US, "%f", domainOfValidity.getMaxY() ) ); //$NON-NLS-1$
    }

    final PdbInfo info = m_connection.getInfo();
    if( info != null )
    {
      final Entry<String, String>[] entries = info.getEntries();
      for( final Entry<String, String> entry : entries )
        properties.setProperty( entry.getKey(), entry.getValue() );
    }

    /* Ask user for missing variables */
    final IWizardPage[] pages = findUpdatePages( scripts, properties );
    if( pages.length > 0 )
    {
      final UpdateScriptWizard wizard = new UpdateScriptWizard( pages );
      wizard.setWindowTitle( WINDOW_TITLE );

      final int open = new WizardDialog( m_shell, wizard ).open();
      if( open != Window.OK )
        throw new CoreException( Status.CANCEL_STATUS );
    }

    updateDependendProperties( properties );

    return properties;
  }

  private IWizardPage[] findUpdatePages( final UpdateScript[] scripts, final Properties properties ) throws CoreException
  {
    final UpdateScriptPageData data = new UpdateScriptPageData( properties );

    // TRICKY: the page name serves as id for the page: if two scripts have pages with the same name,
    // the more recent script should win. Altogether the order of pages should be preserved.
    final Map<String, IWizardPage> pages = new LinkedHashMap<>();

    for( final UpdateScript updateScript : scripts )
    {
      final IWizardPage[] scriptPages = updateScript.createVariablePages( data );
      for( final IWizardPage page : scriptPages )
        pages.put( page.getName(), page );
    }

    return pages.values().toArray( new IWizardPage[pages.size()] );
  }

  private void updateDependendProperties( final Properties properties )
  {
    // FIXME: use the chosen srs in order to set these properties

    properties.setProperty( PdbInfo.PROPERTY_SRS_X_NAME, "X" ); //$NON-NLS-1$
    properties.setProperty( PdbInfo.PROPERTY_SRS_Y_NAME, "Y" ); //$NON-NLS-1$

    // CRS.decode(srid).getCoordinateSystem().getAxis( 0 ).getUnit()
    // for geographic crs use the following
    // properties.setProperty( PdbInfo.PROPERTY_SRS_XName", "Longitude" ); // X is longitude!
    // properties.setProperty( PdbInfo.PROPERTY_SRS_YName", "Latitude" ); // Y is latitude!

    // maybe change (enlarge) x and y tolerance for geographic crs
    properties.setProperty( PdbInfo.PROPERTY_SRS_TOL_X, String.format( Locale.US, "%f", 0.0005 ) ); //$NON-NLS-1$
    properties.setProperty( PdbInfo.PROPERTY_SRS_TOL_Y, String.format( Locale.US, "%f", 0.0005 ) ); //$NON-NLS-1$
    properties.setProperty( PdbInfo.PROPERTY_SRS_TOL_Z, String.format( Locale.US, "%f", 0.0005 ) ); //$NON-NLS-1$
    properties.setProperty( PdbInfo.PROPERTY_SRS_Z_NAME, "Z" ); // equal for all coordinate systems //$NON-NLS-1$
  }

}