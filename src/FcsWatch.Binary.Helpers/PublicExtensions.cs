using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;

namespace FcsWatch.Binary.Helpers
{
    public static class PublicExtensions
    {
        public static void KillTree(this Process process) => Microsoft.Extensions.Internal.ProcessExtensions.KillTree(process);
    }
}
