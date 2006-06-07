/**
 * 
 */
package org.kalypso.model.wspm.ui.profil.wizard.pointsInsert;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIExtensions;


/**
 * @author Belger
 */
public class PointsSourceChooserPage extends WizardPage
{
  private static final String DLG_SETTINGS_SOURCE_ID = PointsSourceChooserPage.class.getName() + ".dialogSettings.selectedSourceID";

  private final IPointsSource[] m_sources = KalypsoModelWspmUIExtensions.createProfilPointSources();

  private IPointsSource m_selectedSource;

  private Group m_sourceStack;

  private StackLayout m_stackLayout;

  public PointsSourceChooserPage( )
  {
    super( "pointsSourceChooser", "Wählen Sie die Quelle der Punkte", null );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final String sourceID = getDialogSettings().get( DLG_SETTINGS_SOURCE_ID );

    if( sourceID == null )
      m_selectedSource = m_sources.length == 0 ? null : m_sources[0];
    else
    {
      for( final IPointsSource source : m_sources )
      {
        if( sourceID.equals( source.getID() ) )
        {
          m_selectedSource = source;
          break;
        }
      }
    }

    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new GridLayout( 2, false ) );

    new Label( panel, SWT.NONE ).setText( "Herkunft der Profilpunkte: " );

    final ComboViewer comboViewer = new ComboViewer( panel, SWT.DROP_DOWN | SWT.READ_ONLY );
    comboViewer.setContentProvider( new ArrayContentProvider() );
    comboViewer.setLabelProvider( new LabelProvider() );
    comboViewer.setInput( m_sources );
    comboViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        setSelectedSource( (IPointsSource) selection.getFirstElement() );
      }
    } );

    final Label sep = new Label( panel, SWT.SEPARATOR | SWT.HORIZONTAL );
    sep.setLayoutData( new GridData( GridData.FILL, GridData.CENTER, true, false, 2, 1 ) );

    m_sourceStack = new Group( panel, SWT.NONE );
    m_stackLayout = new StackLayout();
    m_sourceStack.setLayout( m_stackLayout );
    m_sourceStack.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true, 2, 1 ) );

    for( final IPointsSource source : m_sources )
      source.createControl( m_sourceStack );

    if( m_sources.length > 0 )
      comboViewer.setSelection( new StructuredSelection( m_selectedSource ) );

    setControl( panel );
  }

  protected void setSelectedSource( final IPointsSource source )
  {
    m_selectedSource = source;

    m_sourceStack.setText( m_selectedSource == null ? "" : m_selectedSource.getDescription() );
    m_stackLayout.topControl = m_selectedSource == null ? null : m_selectedSource.getControl( getDialogSettings() );
  }

  public IPointsSource getChoosenSource( )
  {
    return m_selectedSource;
  }

  @Override
  public void dispose( )
  {

    if( m_selectedSource != null )
    {
      getDialogSettings().put( DLG_SETTINGS_SOURCE_ID, m_selectedSource.getID() );
      m_selectedSource.saveState( getDialogSettings() );
    }
    super.dispose();
  }
}
