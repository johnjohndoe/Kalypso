package org.kalypso.ui.repository.actions;

import java.text.DateFormat;
import java.text.ParseException;
import java.util.Date;

import org.eclipse.core.runtime.Preferences;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.window.Window;
import org.kalypso.ogc.sensor.view.ObservationCache;
import org.kalypso.repository.IRepository;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.preferences.IKalypsoPreferences;
import org.kalypso.ui.repository.dialogs.DateRangeInputDialog;
import org.kalypso.ui.repository.view.RepositoryExplorerPart;

/**
 * Configure preview daterange for current <code>IRepository</code>.
 * 
 * @author schlienger
 */
public class ConfigurePreviewAction extends AbstractRepositoryExplorerAction
    implements ISelectionChangedListener
{
  private boolean m_useRange;

  private Date m_from;

  private Date m_to;

  private int m_nDays;

  public ConfigurePreviewAction( final RepositoryExplorerPart explorer )
  {
    super( explorer, "Einstellungen", ImageProvider.IMAGE_ZML_REPOSITORY_CONF,
        "Einstellungen der Zeitreihen-Vorschau setzen" );

    explorer.addSelectionChangedListener( this );

    setEnabled( explorer.isRepository( explorer.getSelection() ) != null );
  }

  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  public void run( )
  {
    final IRepository rep = getExplorer().isRepository(
        getExplorer().getSelection() );
    if( rep == null )
      return;

    final DateFormat df = DateFormat.getDateTimeInstance();

    prepareParameters( rep, df );

    final DateRangeInputDialog dlg = new DateRangeInputDialog( getShell(),
        m_useRange, m_from, m_to, m_nDays, df, null );

    if( dlg.open() == Window.OK )
    {
      rep.setProperty( IKalypsoPreferences.USE_RANGE, String.valueOf( dlg
          .isUseRange() ) );

      if( dlg.isUseRange() )
      {
        final Date from = dlg.getDateFrom();
        final Date to = dlg.getDateTo();

        rep.setProperty( IKalypsoPreferences.DATE_FROM, df.format( from ) );
        rep.setProperty( IKalypsoPreferences.DATE_TO, df.format( to ) );
      }
      else
      {
        final int days = dlg.getNumberOfDays();
        
        rep.setProperty( IKalypsoPreferences.NUMBER_OF_DAYS, String
            .valueOf( days ) );
      }

      // clear cache in order to refetch values from server in case date range changed
      ObservationCache.clearCache();
    }
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( final SelectionChangedEvent event )
  {
    setEnabled( getExplorer().isRepository( event.getSelection() ) != null );
  }

  public void dispose( )
  {
    getExplorer().removeSelectionChangedListener( this );
  }

  /**
   * Helper: prepares the paremeters for config dialog
   * 
   * @param rep
   * @param df
   */
  private void prepareParameters( final IRepository rep, final DateFormat df )
  {
    final Preferences prefs = KalypsoGisPlugin.getDefault()
        .getPluginPreferences();

    final String strUseRange = rep.getProperty( IKalypsoPreferences.USE_RANGE );
    if( strUseRange != null )
      m_useRange = Boolean.valueOf( strUseRange ).booleanValue();
    else
      m_useRange = prefs.getBoolean( IKalypsoPreferences.USE_RANGE );

    final String strFrom = rep.getProperty( IKalypsoPreferences.DATE_FROM );
    try
    {
      if( strFrom != null )
        m_from = df.parse( strFrom );
      else
        m_from = df.parse( prefs.getString( IKalypsoPreferences.DATE_FROM ) );
    }
    catch( ParseException e )
    {
      e.printStackTrace();
      m_from = new Date();
    }

    final String strTo = rep.getProperty( IKalypsoPreferences.DATE_TO );
    try
    {
      if( strTo != null )
        m_to = df.parse( strTo );
      else
        m_to = df.parse( prefs.getString( IKalypsoPreferences.DATE_TO ) );
    }
    catch( ParseException e )
    {
      e.printStackTrace();
      m_to = new Date();
    }

    final String defValue = rep
        .getProperty( IKalypsoPreferences.NUMBER_OF_DAYS );
    if( defValue != null )
      m_nDays = Integer.valueOf( defValue ).intValue();
    else
      m_nDays = prefs.getInt( IKalypsoPreferences.NUMBER_OF_DAYS );
  }
}