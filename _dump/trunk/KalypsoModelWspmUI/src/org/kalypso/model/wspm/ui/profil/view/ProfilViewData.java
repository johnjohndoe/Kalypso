package org.kalypso.model.wspm.ui.profil.view;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.eclipse.ui.IMemento;
import org.eclipse.ui.XMLMemento;
import org.kalypso.model.wspm.core.profil.IProfilDevider;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import de.belger.swtchart.layer.IChartLayer;

/**
 * <p>
 * Daten, welche sich die verschiedenen {@link org.kalypso.model.wspm.ui.profil.view.IProfilView}s teilen ,aber nicht ins Profil
 * selbst gehören.
 * </p>
 * <p>
 * Z.B. welcher Layer sind sichtabr, welcher wird editiert etc.
 * </p>
 * 
 * @author Gernot Belger
 * 
 */
public class ProfilViewData
{
  private Document m_document;

  private final IMemento m_legendMemento;

  private final IMemento m_chartMemento;

  protected boolean m_edithorz = false;

  protected boolean m_editvert = true;
  
  private List<IProfilDevider.DEVIDER_TYP> m_visibleDevider = new ArrayList<IProfilDevider.DEVIDER_TYP>();

  private IChartLayer m_activeLayer;

  private Collection<IProfilViewDataListener> m_listener = new ArrayList<IProfilViewDataListener>();

  public ProfilViewData( )
  {
    try
    {
      final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
      final DocumentBuilder builder = factory.newDocumentBuilder();
      m_document = builder.newDocument();
      m_visibleDevider.addAll(Arrays.asList(IProfilDevider.DEVIDER_TYP.values()));
    }
    catch( final ParserConfigurationException e )
    {
      e.printStackTrace();
    }

    final Element legendelement = m_document.createElement( "legendView" );
    m_legendMemento = new XMLMemento( m_document, legendelement );
    final Element chartelement = m_document.createElement( "chartView" );
    m_chartMemento = new XMLMemento( m_document, chartelement );

  }

  public void dispose( )
  {
    m_listener.clear();
  }

  public IMemento getLegendMemento( )
  {
    return m_legendMemento;
  }

  public IMemento getChartMemento( )
  {
    return m_chartMemento;
  }

  public boolean isEdithorz( )
  {
    return m_edithorz;
  }

  public void setEdithorz( boolean edithorz )
  {
    m_edithorz = edithorz;
  }
  public void setDeviderVisibility( final IProfilDevider.DEVIDER_TYP deviderTyp, final boolean visible )
  {
    if( visible )
    {
      if( !m_visibleDevider.contains( deviderTyp ) )
      {
        m_visibleDevider.add( deviderTyp );
      }

    }
    else
      m_visibleDevider.remove( deviderTyp );
  }

  public boolean getDeviderVisibility( final IProfilDevider.DEVIDER_TYP deviderTyp )
  {

    return m_visibleDevider.contains( deviderTyp );

  }
  public boolean isEditvert( )
  {
    return m_editvert;
  }

  public void setEditvert( boolean editvert )
  {
    m_editvert = editvert;
  }

  public void addProfilViewDataListener( final IProfilViewDataListener l )
  {
    m_listener.add( l );
  }

  public void removeProfilViewDataListener( final IProfilViewDataListener l )
  {
    m_listener.remove( l );
  }

  private void fireChanged( )
  {
    final IProfilViewDataListener[] listeners = m_listener.toArray( new IProfilViewDataListener[m_listener.size()] );
    for( final IProfilViewDataListener l : listeners )
      l.onProfilViewDataChanged();
  }

  public IChartLayer getActiveLayer( )
  {
    return m_activeLayer;
  }

  public void setActiveLayer( IChartLayer activeLayer )
  {
    m_activeLayer = activeLayer;

    fireChanged();
  }

}
