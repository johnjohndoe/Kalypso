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
package org.deegree_impl.model.resources;

// Standard set of Java objects.
import java.lang.Number;
import java.lang.Long;
import java.util.Date;


/**
 * Transforme un objet d'une classe vers une autre. Cette classe sert principalement
 * à convertir en {@link Number} des objets d'une autre classe, par exemple {@link Date}. Une méthode
 * statique, {@link #toNumber}, se charge d'effectuer ce genre de conversion en prenant en compte toutes
 * les classes qui auront été déclarées à <code>ClassChanger</code>.
 * <br><br>
 * Pour déclarer une nouvelle classe, on peut procéder comme suit. L'exemple ci-dessous
 * inscrit une classe qui convertira des objets {@link Date} en objets {@link Long}. Notez qu'il ne s'agit
 * que d'un exemple. Ce convertisseur n'a pas besoin d'être déclaré car <code>ClassChanger</code> comprend
 * déjà les objets {@link Date} par défaut.</p>
 *
 * <blockquote><pre>
 * &nbsp;ClassChanger.register(new ClassChanger(Date.class, Long.class)
 * &nbsp;{
 * &nbsp;    protected Number convert(final Comparable o)
 * &nbsp;    {return new Long(((Date) o).getTime());}
 * &nbsp;
 * &nbsp;    protected Comparable inverseConvert(final Number number)
 * &nbsp;    {return new Date(number.longValue());}
 * &nbsp;});
 * </pre></blockquote>
 *
 * @version 1.0
 * @author Martin Desruisseaux
 */
public abstract class ClassChanger
{
    /**
     * Liste des classes d'objets pouvant être convertis en nombre. Cette liste contiendra
     * par défaut quelques instances de {@link ClassChanger} pour quelques classes standards
     * du Java, telle que {@link Date}. Toutefois, d'autres objets pourront être ajoutés par
     * la suite. Cette liste est <u>ordonnée</u>. Les classe le plus hautes dans la hierarchie
     * (les classes parentes) doivent apparaître à la fin.
     */
    private static ClassChanger[] list=new ClassChanger[]
    {
        new ClassChanger(Date.class, Long.class)
        {
            protected Number convert(final Comparable object)
            {return new Long(((Date) object).getTime());}

            protected Comparable inverseConvert(final Number value)
            {return new Date(value.longValue());}
        }
    };

    /**
     * Parent class for {@link #convert}'s input objects.
     */
    private final Class source;

    /**
     * Parent class for {@link #convert}'s output objects.
     */
    private final Class target;

    /**
     * Construct a new class changer.
     *
     * @param source Parent class for {@link #convert}'s input objects.
     * @param target Parent class for {@link #convert}'s output objects.
     */
    protected ClassChanger(final Class source, final Class target)
    {
        this.source = source;
        this.target = target;
        if (!Comparable.class.isAssignableFrom(source))
        {
            throw new IllegalArgumentException(String.valueOf(source));
        }
        if (!Number.class.isAssignableFrom(target))
        {
            throw new IllegalArgumentException(String.valueOf(target));
        }
    }

    /**
     * Returns the numerical value for an object.
     *
     * @param  object Object to convert (may be null).
     * @return The object's numerical value.
     * @throws ClassCastException if <code>object</code> is not of the expected class.
     */
    protected abstract Number convert(final Comparable object) throws ClassCastException;

    /**
     * Returns an instance of the converted classe from a numerical value.
     *
     * @param  value The value to wrap.
     * @return An instance of the source classe.
     */
    protected abstract Comparable inverseConvert(final Number value);

    /**
     * Returns a string representation for this class changer.
     */
    public String toString()
    {return "ClassChanger["+source.getName()+"\u00A0\u21E8\u00A0"+target.getName()+']';}

    /**
     * Inscrit un nouvel objet <code>ClassChanger</code>. Les objets <code>ClassChanger</code> inscrits
     * ici seront pris en compte par la méthode {@link #toNumber}. Si un objet <code>ClassChanger</code>
     * existait déjà pour une même classe, une exception sera lancée. Cette spécification est justifiée
     * par le fait qu'on enregistre souvent un objet <code>ClassChanger</code> lors de l'initialisation
     * d'une classe qui vient d'être chargée pour la première fois. En interdisant tout changements aux
     * objets <code>ClassChanger</code> après l'initialisation d'une classe, on évite que la façon de
     * convertir des objets en nombres réels ne change au cours d'une exécution de la machine virtuelle.
     * Notez que si <code>converter</code> ne peut pas prendre en charge une même classe que celle d'un
     * autre objet <code>ClassChanger</code>, il peut toutefois prendre en charge une classe parente ou
     * une classe fille.
     *
     * @param  converter Convertisseur à ajouter à la liste des convertisseurs déjà existants.
     * @throws IllegalStateException si un autre objet <code>ClassChanger</code> prennait déjà
     *         en charge la même classe (l'argument <code>classe</code> déclaré au constructeur)
     *         que <code>converter</code>.
     */
    public static synchronized void register(final ClassChanger converter) throws IllegalStateException
    {
        int i;
        for (i=0; i<list.length; i++)
        {
            if (list[i].source.isAssignableFrom(converter.source))
            {
                /*
                 * On a trouvé un convertisseur qui utilisait
                 * une classe parente. Le nouveau convertisseur
                 * devra s'insérer avant son parent. Mais on va
                 * d'abord s'assurer qu'il n'existait pas déjà
                 * un convertisseur pour cette classe.
                 */
                for (int j=i; j<list.length; j++)
                {
                    if (list[j].source.equals(converter.source))
                    {
                        throw new IllegalStateException(list[j].toString());
                    }
                }
                break;
            }
        }
        list = (ClassChanger[]) XArray.insert(list, i, 1);
        list[i] = converter;
    }

    /**
     * Returns the class changer for the specified classe.
     *
     * @throws ClassNotFoundException if <code>source</code> is not a registered class.
     */
    private static synchronized ClassChanger getClassChanger(final Class source) throws ClassNotFoundException
    {
        for (int i=0; i<list.length; i++)
            if (list[i].source.isAssignableFrom(source))
                return list[i];
        throw new ClassNotFoundException(source.getName());
    }

    /**
     * Returns the target class for the specified source class, if a suitable transformation is known.
     * The source class is a {@link Comparable} subclass that will be specified as input to {@link #convert}.
     * The target class is a {@link Number}     subclass that wimm be returned as output by {@link #convert}.
     * If no suitable mapping is found, then <code>source</code> is returned.
     */
    public static Class getTransformedClass(final Class source)
    {
        if (source!=null)
            for (int i=0; i<list.length; i++)
                if (list[i].source.isAssignableFrom(source))
                    return list[i].target;
        return source;
    }

    /**
     * Returns the numeric value for the specified object. For example the code
     * <code>toNumber(new&nbsp;Date())</code> returns the {@link Date#getTime()}
     * value of the specified date object as a {@link Long}.
     *
     * @param  object Object to convert (may be null).
     * @return <code>null</code> if <code>object</code> was null; otherwise
     *         <code>object</code> if the supplied object is already an instance
     *         of {@link Number}; otherwise a new number with the numerical value.
     * @throws ClassNotFoundException if <code>object</code> is not an instance of a registered class.
     */
    public static Number toNumber(final Comparable object) throws ClassNotFoundException
    {
        if (object!=null)
        {
            if (object instanceof Number)
            {
                return (Number) object;
            }
            return getClassChanger(object.getClass()).convert(object);
        }
        return null;
    }

    /**
     * Wrap the specified number as an instance of the specified classe.
     * For example <code>toComparable(Date.class,&nbsp;new&nbsp;Long(time))</code>
     * is equivalent to <code>new&nbsp;Date(time)</code>. There is of course no
     * point to use this method if the destination class is know at compile time.
     * This method is useful for creating instance of classes choosen dynamically
     * at run time.
     *
     * @param  value  The numerical value (may be null).
     * @param  classe The desired classe for return value.
     * @throws ClassNotFoundException if <code>classe</code> is not a registered class.
     */
    public static Comparable toComparable(final Number value, final Class classe) throws ClassNotFoundException
    {
        if (value!=null)
        {
            if (Number.class.isAssignableFrom(classe))
            {
                return (Comparable)value;
            }
            return getClassChanger(classe).inverseConvert(value);
        }
        return null;
    }
}
