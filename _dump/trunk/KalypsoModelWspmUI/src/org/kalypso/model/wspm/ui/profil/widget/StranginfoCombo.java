package org.kalypso.model.wspm.ui.profil.widget;

import org.eclipse.jface.action.IContributionItem;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.kalypso.contribs.eclipse.jface.action.DropdownContributionItem;
import org.kalypso.model.wspm.core.strang.IStranginfoListener;
import org.kalypso.model.wspm.core.strang.ProfilInfo;
import org.kalypso.model.wspm.core.strang.StrangInfo;


/**
 * @author gernot
 *
 */
public class StranginfoCombo implements IStranginfoListener, SelectionListener
{
  private DropdownContributionItem<ProfilInfo> m_comboitem = new DropdownContributionItem<ProfilInfo>( null );

  private StrangInfo m_info = null;

  public StranginfoCombo( )
  {
    m_comboitem.addSelectionListener( this );
  }
  
  public void setInfo( final StrangInfo info )
  {
    if( m_info != null )
      m_info.removeStranginfoListener( this );
    
    m_info = info;
      
    if( m_info == null )
      m_comboitem.setItems( null );
    else
    {
      m_info.addStranginfoListener( this );
      
      m_comboitem.setItems( m_info.getInfos() );
      onIndexChanged( m_info );
    }
  }

  /**
   * @see org.kalypso.model.wspm.core.strang.IStranginfoListener#onIndexChanged(org.kalypso.model.wspm.core.strang.StrangInfo)
   */
  public void onIndexChanged( final StrangInfo source )
  {
    if( source != null )
    {
      final ProfilInfo oldItem = m_comboitem.getSelectedItem();
      if( oldItem != m_info.getInfo() )
        m_comboitem.setSelectedItem( m_info.getInfo() );
    }
  }

  public IContributionItem getItem( )
  {
    return m_comboitem;
  }

  /**
   * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
   */
  public void widgetSelected( final SelectionEvent e )
  {
    if( m_info == null )
      return;
    
    final ProfilInfo selectedItem = m_comboitem.getSelectedItem();

    m_info.setInfo( selectedItem );
    
    onIndexChanged( m_info );
  }

  /**
   * @see org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
   */
  public void widgetDefaultSelected( SelectionEvent e )
  {
  }

  /**
   * @see org.kalypso.model.wspm.core.strang.IStranginfoListener#onTryChangeIndex(org.kalypso.model.wspm.core.strang.StrangInfo)
   */
  public boolean onTryChangeIndex( StrangInfo source )
  {
    return true;
  }
}
