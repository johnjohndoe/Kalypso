/*--------------- Kalypso-Header ------------------------------------------

This file is part of kalypso.
Copyright (C) 2004, 2005 by:

Technical University Hamburg-Harburg (TUHH)
Institute of River and coastal engineering
Denickestr. 22
21073 Hamburg, Germany
http://www.tuhh.de/wb

and

Bjoernsen Consulting Engineers (BCE)
Maria Trost 3
56070 Koblenz, Germany
http://www.bjoernsen.de

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

Contact:

E-Mail:
belger@bjoernsen.de
schlienger@bjoernsen.de
v.doemming@tuhh.de

--------------------------------------------------------------------------*/

package org.kalypso.ogc.gml.featureview.dialog;

import java.util.Collection;

import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.ogc.gml.featureview.FeatureChange;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypsodeegree.model.feature.Feature;

/**
 * A fake dialog, which instead of opening a Dialog fire the requestOpenDialog event.
 *
 * @author belger
 */
public class JumpToFeatureDialog implements IFeatureDialog
{
  private final Feature m_feature;
  private final IFeatureChangeListener m_listener;
  private final IPropertyType m_ftp;

  public JumpToFeatureDialog( final IFeatureChangeListener listener, final Feature feature, final IPropertyType ftp )
  {
    m_listener = listener;
    m_feature = feature;
    m_ftp = ftp;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#open(org.eclipse.swt.widgets.Shell)
   */
  public int open( final Shell shell )
  {
    m_listener.openFeatureRequested( m_feature, m_ftp );
    
    // always return cancel, nothing should be done else
    return Window.CANCEL;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#collectChanges(java.util.Collection)
   */
  public void collectChanges( final Collection<FeatureChange> c )
  {
    // nothing to collect
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#getLabel()
   */
  public String getLabel()
  {
    return m_feature == null ? "<Link nicht gesetzt oder nicht gültig>" : "Link zu Feature: " + m_feature.getId();
  }

}
