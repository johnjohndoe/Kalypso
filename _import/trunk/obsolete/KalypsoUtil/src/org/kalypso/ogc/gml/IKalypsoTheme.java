/*
 * Created on 05.08.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.kalypso.ogc.gml;

import java.awt.Graphics;

import org.deegree.graphics.sld.UserStyle;
import org.kalypso.ogc.MapModell;
import org.kalypso.ogc.event.ModellEventListener;
import org.kalypso.ogc.event.ModellEventProvider;

/**
 * @author Katharina <a href="mailto:k.lupp@web.de>Katharina Lupp</a>
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public interface IKalypsoTheme extends ModellEventProvider, ModellEventListener
{
    /**
     * returns the name of the layer
     */
    public abstract String getName();

    public abstract void setName(final String name);

    /**
     * renders the layer to the submitted graphic context
     */
    public abstract void paint(Graphics g);

    public abstract void paintSelected(Graphics g, int selectionId);

    //  /**
    public abstract UserStyle[] getStyles();

    public abstract void addStyle(final KalypsoUserStyle style);

    public abstract void removeStyle(final KalypsoUserStyle style);

    /**
     * returns the layer that holds the data of the theme
     */
    public abstract KalypsoFeatureLayer getLayer();

    public abstract void setParent(MapModell parent);
}