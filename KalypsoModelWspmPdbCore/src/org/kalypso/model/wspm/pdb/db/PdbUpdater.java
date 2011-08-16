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
import org.kalypso.core.status.StatusDialog2;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PDBRole;
import org.kalypso.model.wspm.pdb.db.version.UpdateScript;
import org.kalypso.model.wspm.pdb.db.version.UpdateScriptExtenions;
import org.kalypso.model.wspm.pdb.db.version.UpdateScriptPageData;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
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

  private final static String WINDOW_TITLE = "Open Connection";

  private static final String STR_CONNECTION_IMPOSSIBLE = "Connection failed: ";

  private static final String STR_CREATE = STR_CONNECTION_IMPOSSIBLE + "database seems to be empty.";

  private static final String STR_UPDATE = STR_CONNECTION_IMPOSSIBLE + "database version (%s) is older than the current version (%s).";

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
        final String msg = String.format( STR_CONNECTION_IMPOSSIBLE + "database version (%s) is newer than the version of Kalypso.\nPlease update Kalypso.", version );
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
    final String msg = String.format( "%s\nDo you wish to create the database?", STR_CREATE );
    final MessageDialog dialog = new MessageDialog( m_shell, WINDOW_TITLE, null, msg, MessageDialog.CONFIRM, new String[] { "Create Now!", IDialogConstants.CANCEL_LABEL }, 1 );
    if( dialog.open() != Window.OK )
      return Status.CANCEL_STATUS;

    /* Do update */
    final String dbType = m_connection.getSettings().getType();
    final UpdateScript createScript = UpdateScriptExtenions.getScript( new Version( 0, 0, 0 ), dbType );
    if( createScript == null )
      throw new IllegalStateException( "Missing create script" ); //$NON-NLS-1$
    final UpdateScript[] scripts = new UpdateScript[] { createScript };
    return executeScripts( scripts, "Create Database" );
  }

  private IStatus handleShouldCreate( )
  {
    final String msg = String.format( "%s\nLog-in with user '%s' to create the database.", STR_CREATE, IPdbConnection.SUPERUSER );
    MessageDialog.openWarning( m_shell, WINDOW_TITLE, msg );
    return Status.CANCEL_STATUS;
  }

  private IStatus handleUpdate( final Version version )
  {
    // 0) ggf. check preconditions

    /* ask user what to do */
    final String baseMsg = String.format( STR_UPDATE, version, PdbInfo.CURRENT_VERSION );
    final String msg = baseMsg + "\nPlease backup the database before updating.\nUpdate database now?";
    final MessageDialog dialog = new MessageDialog( m_shell, WINDOW_TITLE, null, msg, MessageDialog.CONFIRM, new String[] { "Update Now!", IDialogConstants.CANCEL_LABEL }, 1 );
    if( dialog.open() != Window.OK )
      return Status.CANCEL_STATUS;

    /* Do update */
    final String dbType = m_connection.getSettings().getType();
    final UpdateScript[] scripts = UpdateScriptExtenions.getUpdateScripts( version, dbType );
    if( scripts == null )
      throw new IllegalStateException( "Missing create script" ); //$NON-NLS-1$
    return executeScripts( scripts, "Update Database" );
  }

  private IStatus handleShouldUpdate( final Version version )
  {
    final String baseMsg = String.format( STR_UPDATE, version, PdbInfo.CURRENT_VERSION );
    final String msg = String.format( "%s\nLog-in with user '%s' to update.", baseMsg, IPdbConnection.SUPERUSER );
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
        new StatusDialog2( m_shell, status, windowTitle ).open();

      m_connection.updateInfo();

      return status;
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
      return e.getStatus();
    }
  }

  private String[] loadSql( final UpdateScript[] scripts, final Properties variables )
  {
    try
    {
      final PatternInputReplacer<Object> inputReplacer = configurePatternReplacer( variables );

      final Collection<String> sql = new ArrayList<String>();
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
      throw new IllegalStateException( "Failed to load update scripts", e );
    }
  }

  private PatternInputReplacer<Object> configurePatternReplacer( final Properties variables )
  {
    final PatternInputReplacer<Object> inputReplacer = new PatternInputReplacer<Object>( "${", "}" );

    final Set<String> names = variables.stringPropertyNames();
    for( final String name : names )
    {
      final String value = variables.getProperty( name );
      inputReplacer.addReplacer( new ConstantReplacer( name, value ) );
    }

    return inputReplacer;
  }

  private Properties determineVariables( final UpdateScript[] scripts ) throws CoreException
  {
    final Properties properties = new Properties();
    /* some defaults */
    properties.setProperty( PdbInfo.PROPERTY_DOCUMENT_SERVER, "http://example.com/document/path" ); //$NON-NLS-1$
    properties.setProperty( PdbInfo.PROPERTY_SRID, "31468" ); //$NON-NLS-1$
    /* Add depending variables */
    final String srid = properties.getProperty( PdbInfo.PROPERTY_SRID );
    final Envelope domainOfValidity = m_connection.getCrsEnvelope( Integer.valueOf( srid ) );

    if( domainOfValidity != null )
    {
      properties.setProperty( "srsMinX", String.format( Locale.US, "%f", domainOfValidity.getMinX() ) );
      properties.setProperty( "srsMaxX", String.format( Locale.US, "%f", domainOfValidity.getMaxX() ) );

      properties.setProperty( "srsMinY", String.format( Locale.US, "%f", domainOfValidity.getMinY() ) );
      properties.setProperty( "srsMaxY", String.format( Locale.US, "%f", domainOfValidity.getMaxY() ) );

      properties.setProperty( "srsXName", "X" );
      properties.setProperty( "srsYName", "Y" );

// CRS.decode(srid).getCoordinateSystem().getAxis( 0 ).getUnit()

      // for geographic crs use the following
      // properties.setProperty( "srsXName", "Longitude" ); // X is longitude!
      // properties.setProperty( "srsYName", "Latitude" ); // Y is latitude!

      properties.setProperty( "srsZName", "Z" ); // equal for all coordinate systems

      properties.setProperty( "srsMinZ", String.format( Locale.US, "%f", -1000.0 ) );
      properties.setProperty( "srsMaxZ", String.format( Locale.US, "%f", 10000.0 ) );

      // maybe change (enlarge) x and y tolerance for geographic crs
      properties.setProperty( "srsTolX", String.format( Locale.US, "%f", 0.0005 ) );
      properties.setProperty( "srsTolY", String.format( Locale.US, "%f", 0.0005 ) );
      properties.setProperty( "srsTolZ", String.format( Locale.US, "%f", 0.0005 ) );
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
      if( new WizardDialog( m_shell, wizard ).open() != Window.OK )
        throw new CoreException( Status.CANCEL_STATUS );
    }

    return properties;
  }

  private IWizardPage[] findUpdatePages( final UpdateScript[] scripts, final Properties properties ) throws CoreException
  {
    final UpdateScriptPageData data = new UpdateScriptPageData( properties );

    // TRICKY: the page name serves as id for the page: if two scripts have pages with the same name,
    // the more recent script should win. Altogether the order of pages should be preserved.
    final Map<String, IWizardPage> pages = new LinkedHashMap<String, IWizardPage>();

    for( final UpdateScript updateScript : scripts )
    {
      final IWizardPage[] scriptPages = updateScript.createVariablePages( data );
      for( final IWizardPage page : scriptPages )
        pages.put( page.getName(), page );
    }

    return pages.values().toArray( new IWizardPage[pages.size()] );
  }
}