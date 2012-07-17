//---------------------------------------------------------------------------
#ifndef rrCapabilityH
#define rrCapabilityH
#include "rrObject.h"
//---------------------------------------------------------------------------
namespace rr
{

class RR_DECLSPEC Capability : public rrObject
{
    protected:

    public:
        string Hint;
        string Name;
        string Type;
        string Value;

    public:



        /// <summary>
        /// Initializes a new instance of the Capability class.
        /// </summary>
        Capability(const string& name, const string& value, const string& hint, const string& type);

//
//            public Capability(string name, object value, string hint, string type)
//            {
//                Hint = hint;
//                Name = name;
//                Type = type;
//                Value = value.ToString();
//            }
//

//            /// <summary>
//            /// Initializes a new instance of the Capability class.
//            /// </summary>
//            public Capability(string name, object value, string hint)
//            {
//                Hint = hint;
//                Name = name;
//                Value = value.ToString();
//                switch (value.GetType().ToString())
//                {
//                    case "System.Boolean": Type = "int";
//                        BoolValue = (bool)value;
//                        break;
//                    case "System.Int32":
//                    case "System.Int16":
//                    case "System.Int64":
//                        Type = "integer"; break;
//                    case "System.Double":
//                    case "System.Single":
//                        Type = "double"; break;
//                    default: break;
//                }
//            }
//

//            public Capability(XmlNode capNode)
//            {
//                XmlAttribute attr;
//                for (int k1 = 0; k1 < capNode.Attributes.Count; k1++)
//                {
//                    attr = capNode.Attributes.Item(k1) as XmlAttribute;
//                    switch (attr.Name)
//                    {
//                        case "name":
//                            Name = attr.Value;
//                            break;
//                        case "value":
//                            Value = attr.Value;
//                            break;
//                        case "hint":
//                            Hint = attr.Value;
//                            break;
//                        case "type":
//                            Type = attr.Value;
//                            break;
//                    }
//                }
//            }
//
//            public double DoubleValue
//            {
//                get
//                {
//                    double value; double.TryParse(Value, out value);
//                    return value;
//                }
//                set
//                {
//                	 Value = value.ToString();
//                }
//            }
//
//            public int IntValue
//            {
//                get
//                {
//                    int value; int.TryParse(Value, out value);
//                    return value;
//                }
//                set
//                {
//                	Value = value.ToString();
//                }
//            }
//
//            public bool BoolValue
//            {
//                get
//                {
//                    int value; int.TryParse(Value, out value);
//                    return value == 1;
//                }
//                set
//                {
//                	Value= value ? "1" : "0";
//                }
//            }
//
//            public static implicit operator int(Capability c)
//            {
//                return c.IntValue;
//            }
//
//            public static implicit operator double(Capability c)
//            {
//                return c.DoubleValue;
//            }
//
//            public static implicit operator bool (Capability c)
//                {
//                	return c.BoolValue;
//                }
//            public void WriteTo(XmlWriter writer)
//            {
//                writer.WriteStartElement("cap");
//                writer.WriteAttributeString("name", Name);
//                writer.WriteAttributeString("value", Value);
//                writer.WriteAttributeString("hint", Hint);
//                writer.WriteAttributeString("type", Type);
//                writer.WriteEndElement();
//            }
//        }
};

}
#endif
