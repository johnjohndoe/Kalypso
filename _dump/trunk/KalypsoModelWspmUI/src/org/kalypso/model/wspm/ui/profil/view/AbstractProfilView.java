package org.kalypso.model.wspm.ui.profil.view;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfilListener;
import org.kalypso.model.wspm.core.result.IStationResult;


/**
 * @author belger
 */
public abstract class AbstractProfilView implements IProfilListener, IProfilView, IProfilViewDataListener
{
  private final ProfilViewData m_viewdata;

  protected final IProfilEventManager m_pem;

  private Control m_control;

  private final IStationResult[] m_results;
 
  public AbstractProfilView( final IProfilEventManager pem, final ProfilViewData viewdata, final IStationResult[] results )
  {
    m_pem = pem;
    m_viewdata = viewdata;
    m_results = results == null ? new IStationResult[0] : results;
    
    if( m_pem != null )
      m_pem.addProfilListener( this );
    
    if( m_viewdata != null )
      m_viewdata.addProfilViewDataListener( this );
  }
  
  /**
   * @see org.kalypso.model.wspm.ui.profil.view.IProfilView#dispose()
   */
  public void dispose( )
  {
    if( m_pem != null )
      m_pem.removeProfilListener( this );

    if( m_viewdata != null )
      m_viewdata.removeProfilViewDataListener( this );
  }

  protected abstract Control doCreateControl( final Composite parent, final int style );

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.IProfilView#createControl(org.eclipse.swt.widgets.Composite, int)
   */
  public final Control createControl( final Composite parent, final int style )
  {
    m_control = doCreateControl( parent, style );
    return m_control;
  }

  public final Control getControl()
  {
    if( m_control == null )
      throw new IllegalStateException( "createControl not yet called" );
    
    return m_control;
  }
  
  /**
   * @see org.kalypso.model.wspm.ui.profil.view.IProfilView#getProfil()
   */
  public final IProfil getProfil( )
  {
    return m_pem == null ? null : m_pem.getProfil();
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.IProfilView#getViewData()
   */
  public final ProfilViewData getViewData( )
  {
    return m_viewdata;
  }
  
  public IStationResult[] getResults( )
  {
    return m_results;
  }

  public void onProfilViewDataChanged( )
  {
  }
  
  public IProfilEventManager getProfilEventManager( )
  {
    return m_pem;
  }
}
