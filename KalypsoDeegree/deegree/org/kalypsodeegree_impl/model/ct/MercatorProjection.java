/*----------------    FILE HEADER  ------------------------------------------

This file is part of deegree.
Copyright (C) 2001 by:
EXSE, Department of Geography, University of Bonn
http://www.giub.uni-bonn.de/exse/
lat/lon Fitzke/Fretter/Poth GbR
http://www.lat-lon.de

It has been implemented within SEAGIS - An OpenSource implementation of OpenGIS specification
(C) 2001, Institut de Recherche pour le Développement (http://sourceforge.net/projects/seagis/)
SEAGIS Contacts:  Surveillance de l'Environnement Assistée par Satellite
                  Institut de Recherche pour le Développement / US-Espace
                  mailto:seasnet@teledetection.fr


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

Andreas Poth
lat/lon Fitzke/Fretter/Poth GbR
Meckenheimer Allee 176
53115 Bonn
Germany
E-Mail: poth@lat-lon.de

Jens Fitzke
Department of Geography
University of Bonn
Meckenheimer Allee 166
53115 Bonn
Germany
E-Mail: jens.fitzke@uni-bonn.de

                 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.model.ct;

// OpenGIS (SEAS) dependencies
import java.awt.geom.Point2D;
import java.util.Locale;

import org.deegree_impl.model.cs.Projection;
import org.deegree_impl.model.pt.Latitude;
import org.deegree_impl.model.resources.css.ResourceKeys;
import org.deegree_impl.model.resources.css.Resources;


/**
 * Projection cylindrique de Mercator. Les parallèles et les méridients apparaissent
 * comme des lignes droites et se croisent à angles droits; cette projection produit
 * donc des cartes rectangulaires. L'échelle est vrai le long de l'équateur (par défaut)
 * ou le long de deux parallèles équidistants de l'équateur. Cette projection est utilisée
 * pour représenter des régions près de l'équateur. Elle est aussi souvent utilisée pour la
 * navigation maritime parce que toutes les lignes droites sur la carte sont des lignes
 * <em>loxodromiques</em>, c'est-à-dire qu'un navire suivant cette ligne garderait un azimuth
 * constant sur son compas.
 * <br><br>
 *
 * Référence: John P. Snyder (Map Projections - A Working Manual,
 *            U.S. Geological Survey Professional Paper 1395, 1987)
 *
 * @version 1.0
 * @author André Gosselin
 * @author Martin Desruisseaux
 */
final class MercatorProjection extends CylindricalProjection
{
    /**
     * Global scale factor. Value <code>ak0</code>
     * is equals to <code>{@link #a}*k0</code>.
     */
    private final double ak0;

    /**
     * Construct a new map projection from the suplied parameters.
     *
     * @param  parameters The parameter values in standard units.
     * @throws MissingParameterException if a mandatory parameter is missing.
     */
    protected MercatorProjection(final Projection parameters) throws MissingParameterException
    {
        //////////////////////////
        //   Fetch parameters   //
        //////////////////////////
        super(parameters);
        centralLatitude = latitudeToRadians(parameters.getValue("latitude_of_origin", 0), false);
        final double latitudeTrueScale = Math.abs(centralLatitude);

        //////////////////////////
        //  Compute constants   //
        //////////////////////////
        if (isSpherical)
        {
            ak0 = a*Math.cos(latitudeTrueScale);
        }
        else
        {
            ak0 = a*msfn(Math.sin(latitudeTrueScale), Math.cos(latitudeTrueScale));
        }
    }

    /**
     * Returns a human readable name localized for the specified locale.
     */
    public String getName(final Locale locale)
    {return Resources.getResources(locale).getString(ResourceKeys.CYLINDRICAL_MERCATOR_PROJECTION);}

    /**
     * Transforms the specified (<var>x</var>,<var>y</var>) coordinate
     * and stores the result in <code>ptDst</code>.
     */
    protected Point2D transform(double x, double y, final Point2D ptDst) throws TransformException
    {
        if (Math.abs(y) > (Math.PI/2 - EPS))
        {
            throw new TransformException(Resources.format(ResourceKeys.ERROR_POLE_PROJECTION_$1, new Latitude(Math.toDegrees(y))));
        }
        x = (x-centralMeridian)*ak0;
        if (isSpherical)
        {
            y =  ak0*Math.log(Math.tan((Math.PI/4.0) + 0.5*y));
        }
        else
        {
            y = -ak0*Math.log(tsfn(y, Math.sin(y)));
        }
        if (ptDst!=null)
        {
            ptDst.setLocation(x,y);
            return ptDst;
        }
        return new Point2D.Double(x,y);
    }

    /**
     * Transforms the specified (<var>x</var>,<var>y</var>) coordinate
     * and stores the result in <code>ptDst</code>.
     */
    protected Point2D inverseTransform(double x, double y, final Point2D ptDst) throws TransformException
    {
        x = x/ak0 + centralMeridian;
        y = Math.exp(-y/ak0);
        if (isSpherical)
        {
            y = (Math.PI/2.0) - 2.0*Math.atan(y);
        }
        else
        {
            y = cphi2(y);
        }
        if (ptDst!=null)
        {
            ptDst.setLocation(x,y);
            return ptDst;
        }
        return new Point2D.Double(x,y);
    }

    /**
     * Returns a hash value for this projection.
     */
    public int hashCode()
    {
        final long code = Double.doubleToLongBits(ak0);
        return ((int)code ^ (int)(code >>> 32)) + 37*super.hashCode();
    }

    /**
     * Compares the specified object with
     * this map projection for equality.
     */
    public boolean equals(final Object object)
    {
        if (object==this) return true; // Slight optimization
        if (super.equals(object))
        {
            final MercatorProjection that = (MercatorProjection) object;
            return Double.doubleToLongBits(this.ak0) == Double.doubleToLongBits(that.ak0);
        }
        return false;
    }

    /**
     * Informations about a {@link MercatorProjection}.
     *
     * @version 1.0
     * @author Martin Desruisseaux
     */
    static final class Provider extends MapProjection.Provider
    {
        /**
         * Construct a new provider.
         */
        public Provider()
        {super("Mercator_1SP", ResourceKeys.CYLINDRICAL_MERCATOR_PROJECTION);}

        /**
         * Create a new map projection.
         */
        protected Object create(final Projection parameters)
        {return new MercatorProjection(parameters);}
    }
}
