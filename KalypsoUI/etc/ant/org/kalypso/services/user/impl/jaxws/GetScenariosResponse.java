
package org.kalypso.services.user.impl.jaxws;

import javax.xml.bind.annotation.AccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import org.kalypso.services.user.impl.jaxws.GetScenariosResponse;

@XmlRootElement(name = "getScenariosResponse", namespace = "http://impl.user.services.kalypso.org/")
@XmlAccessorType(AccessType.FIELD)
@XmlType(name = "getScenariosResponse", namespace = "http://impl.user.services.kalypso.org/")
public class GetScenariosResponse {

    @XmlElement(name = "return", namespace = "")
    private org.kalypso.services.user.ScenarioBean[] _return;

    /**
     * 
     * @return
     *     returns ScenarioBean[]
     */
    public org.kalypso.services.user.ScenarioBean[] get_return() {
        return this._return;
    }

    /**
     * 
     * @param _return
     *     the value for the _return property
     */
    public void set_return(org.kalypso.services.user.ScenarioBean[] _return) {
        this._return = _return;
    }

}
