/*
 * Map and oceanographical data visualisation Copyright (C) 1999 Pêches et
 * Océans Canada 2000 Institut de Recherche pour le Développement
 * 
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Library General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option) any
 * later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
 * details (http://www.gnu.org/).
 * 
 * 
 * Contacts: FRANCE: Surveillance de l'Environnement Assistée par Satellite
 * Institut de Recherche pour le Développement / US-Espace
 * mailto:seasnet@teledetection.fr
 * 
 * CANADA: Observatoire du Saint-Laurent Institut Maurice-Lamontagne
 * mailto:osl@osl.gc.ca
 */
package javax.units.resources;

/**
 * Noms d'unités en langue française. Les unités qui n'apparaissent pas dans
 * cette ressources ne seront pas localisées.
 * 
 * @version 1.0
 * @author Martin Desruisseaux
 */
public class Units_fr extends Units
{
  /**
   * Liste des unités en français. Les clés sont les symboles standards (sauf
   * exception) des unités. Les valeurs sont les noms en français de ces unités.
   */
  static final String[] contents =
  { "m", "mètre", "g", "gramme", "s", "seconde", "A", "ampère", "K", "kelvin", "mol", "mole", "cd",
      "candela", "rad", "radian", "sr", "stéradian", "Hz", "hertz", "N", "newton", "Pa", "pascal",
      "J", "joule", "W", "watt", "C", "coulomb", "V", "volt", "F", "farad", "\u03A9", "Ohm", "S",
      "siemmens", "T", "tesla", "Wb", "weber", "lx", "lux", "Bq", "becquerel", "Gy", "gray", "Sv",
      "sievert", "H", "henry", "lm", "lumen", "min", "minute", "h", "heure", "d", "jour", "°",
      "degré d'angle", "'", "minute d'angle", "\"", "seconde d'angle", "l", "litre", "L", "litre",
      "t", "tonne métrique", "eV", "électronvolt", "u", "unité de masse atomique unifiée", "ua",
      "unité astronomique", "inch", "pouce", // Symbole non-standard
      "foot", "pied", // Symbole non-standard
      "yard", "yard", // Symbole non-standard
      "fathom", "brasse anglaise", // Symbole non-standard
      "brasse", "brasse française", // Symbole non-standard
      "mile", "mille", // Symbole non-standard
      "nmile", "mille marin", // Symbole non-standard
      "knot", "noeud", // Symbole non-standard
      "are", "are", "ha", "hectare", "bar", "bar", "Å", "ångström", "barn", "barn", "erg", "erg",
      "dyn", "dyne", "P", "poise", "St", "stokes", "G", "gauss", "Oe", "oersted", "Mx", "maxwell",
      "sb", "stilb", "ph", "phot", "Gal", "gal", "Ci", "curie", "R", "röntgen", "rd", "rad", "rem",
      "rem", "Jy", "jansky", "Torr", "torr", "atm", "atmosphère normale", "pound", "livre", // Symbole
                                                                                            // non-standard
      "onze", "onze", // Symbole non-standard
      "°C", "degré celcius", "°F", "fahrenheit" // Symbole non-standard
  };

  /**
   * Initialise les ressources françaises.
   */
  public Units_fr()
  {
    super( contents );
  }
}