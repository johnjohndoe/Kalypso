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
package org.kalypso.model.wspm.ui.view;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.eclipse.ui.IMemento;
import org.eclipse.ui.XMLMemento;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import de.belger.swtchart.layer.IChartLayer;

/**
 * <p>
 * Daten, welche sich die verschiedenen {@link org.kalypso.model.wspm.ui.profil.view.IProfilView}s teilen ,aber nicht
 * ins Profil selbst gehören.
 * </p>
 * <p>
 * Z.B. welcher Layer sind sichtabr, welcher wird editiert etc.
 * </p>
 * 
 * @author Gernot Belger
 */
public class ProfilViewData
{
  private Document m_document;

  private final IMemento m_legendMemento;

  private final IMemento m_chartMemento;

  protected boolean m_edithorz = false;

  protected boolean m_editvert = true;

  protected boolean m_useDeviderValue = false;

  private List<String> m_visibleMarker = new ArrayList<String>();

  private List<String> m_visiblePointProperties = new ArrayList<String>();

  private IChartLayer m_activeLayer;

  private Collection<IProfilViewDataListener> m_listener = new ArrayList<IProfilViewDataListener>();

  public ProfilViewData( )
  {
    try
    {
      final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
      final DocumentBuilder builder = factory.newDocumentBuilder();
      m_document = builder.newDocument();
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

  public boolean isVisible( final String pointProperty )
  {
    return m_visiblePointProperties.contains( pointProperty );
  }

  public void changeVisibility( final String pointProperty, final boolean visibility )
  {
    if( visibility )
    {
      if( !m_visiblePointProperties.contains( pointProperty ) )
        m_visiblePointProperties.add( pointProperty );
    }
    else
    {
      if( m_visiblePointProperties.contains( pointProperty ) )
        m_visiblePointProperties.remove( pointProperty );
    }
  }

  public IMemento getLegendMemento( )
  {
    return m_legendMemento;
  }

  public void useDeviderValue( final boolean use )
  {
    m_useDeviderValue = use;
  }

  public boolean useDeviderValue( )
  {
    return m_useDeviderValue;
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

  public void setMarkerVisibility( final String markerTyp, final boolean visible )
  {
    if( visible )
    {
      if( !m_visibleMarker.contains( markerTyp ) )
      {
        m_visibleMarker.add( markerTyp );
      }

    }
    else
      m_visibleMarker.remove( markerTyp );
  }

  public boolean getDeviderVisibility( final String deviderTyp )
  {

    return m_visibleMarker.contains( deviderTyp );

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
